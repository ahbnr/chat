module MulticastDiscovery where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Maybe (maybe, catMaybes)
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

type Name = String
data Request = Ping deriving (Show, Read)
data Response = Pong String PortNumber deriving (Show, Read)

poolGrp :: String
poolGrp = "230.42.42.42"

listenPort :: PortNumber
listenPort = 4242

queryResponsePort :: PortNumber
queryResponsePort = 4243

queryTimeout :: Microseconds
queryTimeout = 1000000

logID :: String
logID = "MulticastDiscovery"

sendPoolMsg :: String -> IO ()
sendPoolMsg msg =
-- ^send a message into the multicast group used for discovery of peers
  bracket
    (multicastSender poolGrp listenPort)
    (close . fst)
    (\(sock, addr) -> void (sendTo sock (pack msg) addr))

queryPool :: IO [(Name, HostAddress, PortNumber)]
queryPool =
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
          (sendPoolMsg . show) Ping

          -- collect all responses within a fixed timeout
          responses <- gatherInput queryTimeout (recvFrom sock 100)

          (pure . catMaybes . map extractResponse) responses
        )
  where
    extractResponse :: (ByteString, SockAddr) -> Maybe (Name, HostAddress, PortNumber)
    extractResponse (msg, (SockAddrInet _ ip)) =
    -- ^interpret a received message and return data about sender, if valid
      (   fmap (\(Pong name port) -> (name, ip, port))
          . readMaybe
          . unpack
        ) msg
    extractResponse _ = Nothing

listenForPingUnsafe :: Socket -> IO (Maybe HostAddress)
listenForPingUnsafe sock = do
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

answerPingUnsafe :: Socket -> Name -> PortNumber -> HostAddress -> IO ()
answerPingUnsafe sock name tcpPort remoteIp = do
-- ^send a pong to a peer, for the purpose of answering a previous ping.
--  The ping will contain the name of our peer and the tcp port it is listening
--  on for connections
  let responseAddr = (SockAddrInet queryResponsePort remoteIp)

  debugM
    logID
    (concat ["Sending udp ping answer to ", show responseAddr])

  void (
      sendTo 
        sock
        ((pack . show) (Pong name tcpPort))
        responseAddr
    )

pingListenerServiceUnsafe :: Name -> PortNumber -> IO()
pingListenerServiceUnsafe name tcpPort =
-- ^listen for pings and answer them in an endless loop
  (allowCancel . bracket
      (multicastReceiver poolGrp listenPort)
      close
    ) (\sock ->
        (sequence_ . repeat)
          (do
              maybeAddr <- listenForPingUnsafe sock

              maybe
                (pure ())
                (answerPingUnsafe sock name tcpPort)
                maybeAddr
            )
      )
  
