module BroadcastDiscovery where

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
    , setSocketOption
    , Family(AF_INET)
    , tupleToHostAddress
    , Socket
    , PortNumber
    , SocketOption(ReuseAddr, Broadcast)
  )

import Network.Socket.ByteString (sendTo, recvFrom)
import Network.Multicast (multicastSender, multicastReceiver)

import System.Log.Logger (debugM)

import Utils (gatherInput, Microseconds, allowCancel)

import Discovery (Name, Request(Ping), Response(Pong), DiscoveryReceiver, DiscoverySender, senderTemplate, receiverTemplate)

broadcastAddr :: HostAddress
broadcastAddr = tupleToHostAddress (255, 255, 255, 255)

listenPort :: PortNumber
listenPort = 4244

listenAddr :: HostAddress
listenAddr = tupleToHostAddress (0, 0, 0, 0)

queryResponsePort :: PortNumber
queryResponsePort = 4245

queryTimeout :: Microseconds
queryTimeout = 1000000

logID :: String
logID = "BroadcastDiscovery"

sendQuery :: String -> IO ()
sendQuery msg =
-- ^broadcast a message for discovery of peers. If someone receives it, they can answer to establish communication.
  bracket
    (socket AF_INET Datagram defaultProtocol)
    close
    (\sock -> do
        setSocketOption sock ReuseAddr 1
        setSocketOption sock Broadcast 1

        void (sendTo sock (pack msg) (SockAddrInet listenPort broadcastAddr))
      )

sender :: DiscoverySender
sender = senderTemplate queryResponsePort sendQuery

receiver :: DiscoveryReceiver
receiver name tcpPort =
  (allowCancel . bracket
      (socket AF_INET Datagram defaultProtocol)
      close
    ) (\sock -> do
          setSocketOption sock ReuseAddr 1
          setSocketOption sock Broadcast 1

          bind sock (SockAddrInet listenPort listenAddr)
        
          receiverTemplate sock queryResponsePort name tcpPort
        )
