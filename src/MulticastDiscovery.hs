module MulticastDiscovery where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import Control.Exception (bracket)
import Control.Monad (void)

import Data.Maybe (maybe, mapMaybe)
import Text.Read (readMaybe)

import Network.Socket (
      socket
    , close
    , bind
    , HostAddress
    , SockAddr(SockAddrInet)
    , defaultProtocol
    , SocketType(Datagram)
    , Family(AF_INET)
    , tupleToHostAddress
    , Socket
    , PortNumber
  )

import Network.Socket.ByteString (sendTo, recvFrom)
import Network.Multicast (multicastSender, multicastReceiver)

import System.Log.Logger (debugM)

import Utils (gatherInput, Microseconds, allowCancel)

import Discovery (senderTemplate, receiverTemplate, Name, Request(Ping), Response(Pong), DiscoveryReceiver, DiscoverySender)

poolGrp :: String
poolGrp = "239.42.42.42"

listenPort :: PortNumber
listenPort = 4242

queryResponsePort :: PortNumber
queryResponsePort = 4243

queryTimeout :: Microseconds
queryTimeout = 1000000

logID :: String
logID = "MulticastDiscovery"

sendQuery :: String -> IO ()
sendQuery msg =
-- ^send a message into the multicast group used for discovery of peers
  bracket
    (multicastSender poolGrp listenPort)
    (close . fst)
    (\(sock, addr) -> void (sendTo sock (pack msg) addr))

sender :: DiscoverySender
sender = senderTemplate queryResponsePort sendQuery

receiver :: DiscoveryReceiver
receiver name tcpPort =
  (allowCancel . bracket
      (multicastReceiver poolGrp listenPort)
      close
    ) (\sock -> receiverTemplate sock queryResponsePort name tcpPort)
