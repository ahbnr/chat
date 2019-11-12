module IrcDiscovery where

import System.Log.Logger (debugM)

import qualified Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString (unpack)
import qualified Data.Text (pack, unpack)

import Data.Function ((&))
import Data.List (intersperse)
import Data.Word (Word8, Word32)
import Data.Bits (shiftR)
import Data.Either.Combinators (rightToMaybe)
import Data.Text (replace, Text)
import Data.Maybe (isNothing, catMaybes)

import Text.Read (readMaybe)


import Utils (takeUntilM, gatherInput, Microseconds, allowCancel)

import Discovery (senderTemplate, receiverTemplate, Name, Request(Ping), Response(Pong), DiscoveryReceiver, DiscoverySender)

import Config (IrcConfig (IrcConfig))

import Control.Lens.Operators ((%~), (.~))
import Network.Socket (PortNumber, HostAddress, tupleToHostAddress)
import Network.Info (getNetworkInterfaces, NetworkInterface(NetworkInterface), IPv4(IPv4), ipv4)
import Network.IRC.Client (IRC, EventHandler(EventHandler), matchType, matchNumeric, runClient, runClientWith, runIRCAction, newIRCState, defaultInstanceConfig, tlsConnection, TLSConfig(WithDefaultConfig), channels, handlers, ondisconnect, onconnect, Message(Privmsg), send, logfunc, stdoutLogger)
import Network.IRC.Client.Events (Source(User, Server, Channel))
import Network.IRC.Conduit.Lens (_Privmsg, _Join)
import Network.IRC.CTCP (getUnderlyingByteString)
import Conduit (liftIO, throwM)

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TMChan (TMChan, newTMChan, closeTMChan, writeTMChan, readTMChan)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, tryTakeTMVar, readTMVar)
import Control.Concurrent.STM (STM)
import Control.Monad.STM (atomically)
import Control.Monad.State (get)
import Control.Monad (void)

import Control.Exception (SomeException)

import Data.Conduit.TMChan (sourceTMChan)
import Data.Conduit.List (consume)
import Data.Conduit (runConduit, (.|))

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Numeric (showHex)

import Debug.Trace (traceShowId)

queryTimeout :: Microseconds
queryTimeout = 600000

data IRCRequest = IRCPing deriving (Show, Read)
data IRCResponse = IRCPong String [HostAddress] PortNumber deriving (Show, Read)
-- ^Name [(Addresses, Ports)]

type ClientState = (TMChan IRCResponse, TMVar Bool)
-- ^(collected responses, flag, whether client has connected and joined discovery channel yet)

logID :: String
logID = "IrcDiscovery"

normalizeNick :: String -> Text
-- ^transforms the peer name into a valid IRC nick which is as unique as possible
--  using the BLAKE2b hash function
normalizeNick name = 
  (Data.Text.pack . concat)
    [
        "P", -- irc nicks have to start with a non numeric character
        concatMap (`showHex` "") -- https://stackoverflow.com/a/8416189
          (Data.ByteString.unpack $ hash 7 mempty (Data.ByteString.Char8.pack name))
      ]

sender :: ClientState -> DiscoverySender
sender (discoveryStream, joinedStatus) = do
  debugM
    IrcDiscovery.logID
    "Waiting for the IRC client to be connected and also until it joined the discovery channel."

  joined <- atomically $ readTMVar joinedStatus
  if not joined then do
    debugM
      IrcDiscovery.logID
      "IRC client failed to connect and join channel. Aborting sending IRCPing."
    pure []
  else do
    debugM
      IrcDiscovery.logID
      (concat ["Waiting for results of IRCPing, which will be send by the IRC client thread. Waiting ", show queryTimeout, " microseconds"])

    threadDelay queryTimeout

    debugM
      IrcDiscovery.logID
      "Waiting time is up. Collecting responses."

    atomically $ closeTMChan discoveryStream
    responses <- runConduit $ sourceTMChan discoveryStream .| consume

    debugM
      IrcDiscovery.logID
      (concat ["The following peers responded: ", show [name | (IRCPong name _ _) <- responses]])

    pure [(name, addrs, port) | (IRCPong name addrs port) <- responses]

pingService :: Text -> EventHandler ClientState
pingService nick = EventHandler
    (matchType _Join)
    (\source channelName -> do
        liftIO $ debugM
          IrcDiscovery.logID
          (concat ["Received a JOIN message from ", show source, ". Joined ", Data.Text.unpack channelName])

        case source of
          Channel _ joinedNick
            | (nick == joinedNick) && (channelName == Data.Text.pack "##hs-cli-chat-discovery") -> do
              liftIO $ debugM
                IrcDiscovery.logID
                "Joined the discovery channel. Sending IRCPing."

              send (Privmsg channelName ((Right . Data.Text.pack . show) IRCPing))

              (_, joinedStatus) <- get
              (liftIO . atomically . setTMVar joinedStatus) True

              liftIO $ debugM
                IrcDiscovery.logID
                "Client status set to `joined`"

              pure ()
            | otherwise -> pure ()
          _ -> pure ()
    )

setTMVar :: TMVar a -> a -> STM ()
setTMVar var val = do
  void $ tryTakeTMVar var
  putTMVar var val

pongReceiveService :: EventHandler ClientState
pongReceiveService = EventHandler
    (matchType _Privmsg)
    (\_ msg -> do
        liftIO $ debugM
          IrcDiscovery.logID
          (concat ["pongReceiveService: Received a PRIVMSG: ", show msg])

        case receivePong msg of
          Just response@(IRCPong peerName addresses port) -> do
            liftIO $ debugM
              IrcDiscovery.logID
              (concat ["The PRIVMSG contained an IRCPong from ", peerName, ". Collecting addresses."])

            (chan, _) <- get
            (liftIO . atomically . writeTMChan chan) response
          Nothing -> pure ()
      )
  where
    receivePong (target, eitherCTCPOrMsg) =
      (
            ((>>= readMaybe) :: Maybe String -> Maybe IRCResponse)
          . fmap Data.Text.unpack
          . rightToMaybe
        ) eitherCTCPOrMsg

receiver :: IrcConfig -> ClientState -> DiscoveryReceiver
receiver (IrcConfig ircServer ircPort) initialState name tcpPort =
  let
    nick = normalizeNick name
    conn = tlsConnection (WithDefaultConfig (Data.ByteString.Char8.pack ircServer) ircPort)
            & ondisconnect .~ onDisconnect
            -- & logfunc .~ stdoutLogger
    cfg = defaultInstanceConfig nick
            & channels .~ [Data.Text.pack "##hs-cli-chat-discovery"]
            & handlers %~ (++ [pongService name tcpPort, pingService nick, pongReceiveService])
  in
    allowCancel (do
        liftIO $ debugM
          IrcDiscovery.logID
          (concat ["Starting IRC client. Connecting as ", Data.Text.unpack nick, " to ", ircServer, ":", show ircPort])

        runClient conn cfg initialState
      )

pongService :: String -> PortNumber -> EventHandler a
pongService name tcpPort = EventHandler
    (matchType _Privmsg)
    (\source msg -> do
      liftIO $ debugM
        IrcDiscovery.logID
        (concat ["pongService: Received a PRIVMSG: ", show msg])

      case receivePing source msg of
        Just nick -> do
          liftIO $ debugM
            IrcDiscovery.logID
            (concat ["The received PRIVMSG was an IRCPing from ", Data.Text.unpack nick, ". Answering with IRCPong."])

          addresses <- liftIO getAddresses
          send (Privmsg nick ((Right . Data.Text.pack . show) (IRCPong name addresses tcpPort)))
        _ -> pure ()
    )
  where
    receivePing source (target, eitherCTCPOrMsg) = 
      let
        sourceStr = case source of
          User nick -> nick
          Channel chan nick -> nick
          Server name -> name
      in
        (
              fmap (const sourceStr)
            . ((>>= readMaybe) :: Maybe String -> Maybe IRCRequest)
            . fmap Data.Text.unpack
            . rightToMaybe
          ) eitherCTCPOrMsg

onDisconnect :: Maybe SomeException -> IRC ClientState ()
onDisconnect (Just exc) = do
  liftIO $ debugM
    IrcDiscovery.logID
    (concat ["Disconnecting IRC due to the following exception: ", show exc])

  (_, joinedStatus) <- get
  (liftIO . atomically . setTMVar joinedStatus) False

  throwM exc
onDisconnect Nothing = pure ()

getAddresses :: IO [HostAddress]
getAddresses = fmap (map (tupleToHostAddress . word8s . (\(IPv4 ip) -> ip) . ipv4)) getNetworkInterfaces
  where
    -- Code taken from internal function of network-info:
    word8s :: Word32 -> (Word8, Word8, Word8, Word8)
    word8s x = ( fromIntegral   x
               , fromIntegral $ x `shiftR` 8
               , fromIntegral $ x `shiftR` 16
               , fromIntegral $ x `shiftR` 24 )

genIrcDiscovery :: IrcConfig -> IO (DiscoveryReceiver, DiscoverySender)
genIrcDiscovery ircConfig = do
  discoveryStream <- atomically newTMChan
  joined <- atomically newEmptyTMVar
  let state = (discoveryStream, joined)

  pure (receiver ircConfig state, sender state)
