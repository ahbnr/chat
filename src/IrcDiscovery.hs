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
normalizeNick name = 
  (Data.Text.pack . concat)
    [
        "P",
        (concat . map (flip showHex "")) -- https://stackoverflow.com/a/8416189
          (Data.ByteString.unpack $ hash 7 mempty (Data.ByteString.Char8.pack name))
      ]

sender :: String -> DiscoverySender
sender name =
  let
    normalizedName = normalizeNick (name ++ "SEND")
    conn = plainConnection (Data.ByteString.Char8.pack "chat.freenode.net") 6667
            -- & logfunc .~ stdoutLogger
    cfg = defaultInstanceConfig normalizedName
            & channels .~ [Data.Text.pack "##hs-cli-chat-discovery"]
            & handlers %~ (++ [pingService normalizedName, pongReceiveService])
  in do
    discoveryStream <- atomically newTMChan
    client <- (async . allowCancel) $ runClient conn cfg discoveryStream
    threadDelay queryTimeout
    atomically $ closeTMChan discoveryStream
    cancel client
    responses <- runConduit $ sourceTMChan discoveryStream .| consume
    pure [(name, addrs, port) | (IRCPong name addrs port) <- responses]

pingService :: Text -> EventHandler a
pingService nick = EventHandler
    (matchType _Join)
    (\source channelName -> 
        case source of
          Channel _ joinedNick
            | (nick == joinedNick) && (channelName == (Data.Text.pack "##hs-cli-chat-discovery")) -> do
              send (Privmsg channelName ((Right . Data.Text.pack . show) IRCPing))
            | otherwise -> pure ()
          _ -> pure ()
    )

pongReceiveService :: EventHandler (TMChan IRCResponse)
pongReceiveService = EventHandler
    (matchType _Privmsg)
    (\_ msg -> do
        case receivePong msg of
          Just response@(IRCPong peerName addresses port) -> do
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

receiver :: DiscoveryReceiver
receiver name tcpPort =
  let
    normalizedName = normalizeNick (name ++ "RECV")
    conn = plainConnection (Data.ByteString.Char8.pack "chat.freenode.net") 6667
            -- & logfunc .~ stdoutLogger
    cfg = defaultInstanceConfig normalizedName
            & channels .~ [Data.Text.pack "##hs-cli-chat-discovery"]
            & handlers %~ ((pongService name tcpPort):)
  in do
    allowCancel (
        runClient conn cfg ()
      )

pongService :: String -> PortNumber -> EventHandler ()
pongService name tcpPort = EventHandler
    (matchType _Privmsg)
    (\source msg -> do
      case receivePing source msg of
        Just nick -> do
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
