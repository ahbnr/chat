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

import Control.Lens.Operators ((%~), (.~))
import Network.Socket (PortNumber, HostAddress, tupleToHostAddress)
import Network.Info (getNetworkInterfaces, NetworkInterface(NetworkInterface), IPv4(IPv4), ipv4)
import Network.IRC.Client (EventHandler(EventHandler), matchType, matchNumeric, runClient, runClientWith, runIRCAction, newIRCState, defaultInstanceConfig, plainConnection, channels, handlers, onconnect, Message(Privmsg), send, logfunc, stdoutLogger)
import Network.IRC.Client.Events (Source(User, Server, Channel))
import Network.IRC.Conduit.Lens (_Privmsg, _Join)
import Network.IRC.CTCP (getUnderlyingByteString)
import Conduit (liftIO)

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TMChan (TMChan, newTMChan, closeTMChan, writeTMChan, readTMChan)
import Control.Monad.STM (atomically)
import Control.Monad.State (get)
import Control.Monad (void)

import Data.Conduit.TMChan (sourceTMChan)
import Data.Conduit.List (consume)
import Data.Conduit (runConduit, (.|))

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Numeric (showHex)

import Debug.Trace (traceShowId)

queryTimeout :: Microseconds
queryTimeout = 12000000

data IRCRequest = IRCPing deriving (Show, Read)
data IRCResponse = IRCPong String [HostAddress] PortNumber deriving (Show, Read)
-- ^ Name [(Addresses, Ports)]

logID :: String
logID = "IrcDiscovery"

normalizeNick :: String -> Text
-- ^transforms the peer name into a valid IRC nick which is as unique as possible
--  using the BLAKE2b hash function
normalizeNick name = 
  (Data.Text.pack . concat)
    [
        "P", -- irc nicks have to start with a non numeric character
        (concat . map (flip showHex "")) -- https://stackoverflow.com/a/8416189
          (Data.ByteString.unpack $ hash 7 mempty (Data.ByteString.Char8.pack name))
      ]

sender :: TMChan IRCResponse -> DiscoverySender
sender discoveryStream = do
  debugM
    IrcDiscovery.logID
    (concat ["Waiting for results of IRCPing, which will be send by the IRC client thread. Waiting ", show queryTimeout, " milliseconds"])

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

pingService :: Text -> EventHandler a
pingService nick = EventHandler
    (matchType _Join)
    (\source channelName -> do
        liftIO $ debugM
          IrcDiscovery.logID
          (concat ["Received a JOIN message from ", show source, ". Joined ", Data.Text.unpack channelName])

        case source of
          Channel _ joinedNick
            | (nick == joinedNick) && (channelName == (Data.Text.pack "##hs-cli-chat-discovery")) -> do
              liftIO $ debugM
                IrcDiscovery.logID
                "Joined the discovery channel. Sending IRCPing."

              send (Privmsg channelName ((Right . Data.Text.pack . show) IRCPing))
            | otherwise -> pure ()
          _ -> pure ()
    )

pongReceiveService :: EventHandler (TMChan IRCResponse)
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

            chan <- get
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

receiver :: TMChan IRCResponse -> DiscoveryReceiver
receiver discoveryStream name tcpPort =
  let
    nick = normalizeNick name
    conn = plainConnection (Data.ByteString.Char8.pack "chat.freenode.net") 6667
            -- & logfunc .~ stdoutLogger
    cfg = defaultInstanceConfig nick
            & channels .~ [Data.Text.pack "##hs-cli-chat-discovery"]
            & handlers %~ (++ [pongService name tcpPort, pingService nick, pongReceiveService])
  in do
    allowCancel (do
        liftIO $ debugM
          IrcDiscovery.logID
          (concat ["Starting IRC client. Connecting as ", Data.Text.unpack nick, "."])

        runClient conn cfg discoveryStream
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
      in do
        (
              fmap (const sourceStr)
            . ((>>= readMaybe) :: Maybe String -> Maybe IRCRequest)
            . fmap Data.Text.unpack
            . rightToMaybe
          ) eitherCTCPOrMsg

getAddresses :: IO [HostAddress]
getAddresses = fmap (map (tupleToHostAddress . word8s . (\(IPv4 ip) -> ip) . ipv4)) getNetworkInterfaces
  where
    -- Code taken from internal function of network-info:
    word8s :: Word32 -> (Word8, Word8, Word8, Word8)
    word8s x = ( fromIntegral $ x
               , fromIntegral $ x `shiftR` 8
               , fromIntegral $ x `shiftR` 16
               , fromIntegral $ x `shiftR` 24 )

genIrcDiscovery :: IO (DiscoverySender, DiscoveryReceiver)
genIrcDiscovery = do
  discoveryStream <- atomically newTMChan
  pure (sender discoveryStream, receiver discoveryStream)
