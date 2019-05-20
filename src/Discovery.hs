module Discovery where

import System.Log.Logger (debugM)

import Data.List (nubBy)
import Data.Function ((&))
import Control.Monad (void)
import Control.Exception (bracket)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import Network.Socket (
      HostAddress
    , PortNumber
    , SockAddr(SockAddrInet)
    , Socket
    , socket
    , close
    , SocketType(Datagram)
    , Family(AF_INET)
    , tupleToHostAddress
    , bind
    , defaultProtocol
  )

import Network.Socket.ByteString (sendTo, recvFrom)

import TaskManager (TaskManager, manage, wait, withTaskManager)

import Utils (gatherInput, Microseconds)

type Name = String
data Request = Ping deriving (Show, Read)
data Response = Pong String PortNumber deriving (Show, Read)

type Peer = (Name, [HostAddress], PortNumber)

type DiscoverySender = IO [Peer]
type DiscoveryReceiver = Name -> PortNumber -> IO()
-- ^name of local peer -> port of local chat server -> (execution of discovery receiver)

logID :: String
logID = "Discovery"

discoverPeers :: [DiscoverySender] -> IO [Peer]
discoverPeers =
    fmap (
        nubBy (\(name1, _, _) (name2, _, _) -> name1 == name2)
      . concat
    )
  . sequence

makeVisible :: Name -> PortNumber -> TaskManager () -> [DiscoveryReceiver] -> IO ()
makeVisible name tcpPort tm = void . sequence . map (manage tm . (&) tcpPort . (&) name)

queryTimeout :: Microseconds
queryTimeout = 1000000

senderTemplate :: PortNumber -> (String -> IO ()) -> DiscoverySender
senderTemplate queryResponsePort singleQuery =
-- ^search for peers on the network, return their names and addresses + tcp ports
    bracket
      (socket AF_INET Datagram defaultProtocol)
      close
      (\sock -> do
          -- address on which we will listen for responses to our
          -- query
          let returnAddr = SockAddrInet queryResponsePort (tupleToHostAddress (0, 0, 0, 0))
          bind sock returnAddr

          -- ping all peers (multicast)
          (singleQuery . show) Ping

          -- collect all responses within a fixed timeout
          responses <- gatherInput queryTimeout (recvFrom sock 100)

          (pure . mapMaybe extractResponse) responses
        )
  where
    extractResponse :: (ByteString, SockAddr) -> Maybe (Name, [HostAddress], PortNumber)
    extractResponse (msg, SockAddrInet _ ip) =
    -- ^interpret a received message and return data about sender, if valid
      (   fmap (\(Pong name port) -> (name, [ip], port))
          . readMaybe
          . unpack
        ) msg
    extractResponse _ = Nothing

receiverTemplate :: Socket -> PortNumber -> DiscoveryReceiver
receiverTemplate sock queryResponsePort name tcpPort =
-- ^listen for pings and answer them in an endless loop
  (sequence_ . repeat)
    (do
        maybeAddr <- listenForUDPPingUnsafe sock

        maybe
          (pure ())
          (answerUDPPingUnsafe sock name tcpPort queryResponsePort)
          maybeAddr
      )

listenForUDPPingUnsafe :: Socket -> IO (Maybe HostAddress)
listenForUDPPingUnsafe sock = do
-- ^listen for a ping on the given socket and return the sender address
  debugM
    logID
    "Listening for udp pings..."

  (msg, addr@(SockAddrInet _ remoteIp)) <- recvFrom sock 100

  debugM
    logID
    (concat ["Received ", show msg, " from ", show addr])

  -- return ip of sender of ping, if the received data
  -- is a ping
  let maybeRequest = (readMaybe . unpack) msg :: Maybe Request

  pure $ case maybeRequest of
    Just Ping -> Just remoteIp
    _ -> Nothing

answerUDPPingUnsafe :: Socket -> Name -> PortNumber -> PortNumber -> HostAddress -> IO ()
answerUDPPingUnsafe sock name tcpPort queryResponsePort remoteIp = do
-- ^send a pong to a peer, for the purpose of answering a previous ping.
--  The ping will contain the name of our peer and the tcp port it is listening
--  on for connections
  let responseAddr = SockAddrInet queryResponsePort remoteIp

  debugM
    logID
    (concat ["Sending udp ping answer to ", show responseAddr])

  void (
      sendTo 
        sock
        ((pack . show) (Pong name tcpPort))
        responseAddr
    )
