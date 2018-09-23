module MulticastDiscovery where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import Control.Exception (bracket)
import Data.Maybe
import Text.Read(readMaybe)

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
import Network.Multicast

import System.Log.Logger

import Utils (void, gatherInput, Microseconds)

type Name = String
data Request = Ping deriving (Show, Read)
data Response = Pong String PortNumber deriving (Show, Read)

poolGrp :: String
poolGrp = "230.42.42.42"

listenPort :: Integral a => a
listenPort = 4242

queryResponsePort :: Integral a => a
queryResponsePort = 4243

queryTimeout :: Microseconds
queryTimeout = 1000000

logID :: String
logID = "MulticastDiscovery"

sendPoolMsg :: String -> IO ()
sendPoolMsg msg = do
  (sock, addr) <- multicastSender poolGrp listenPort

  sendTo sock (pack msg) addr

  close sock

queryPool :: IO [(Name, HostAddress, PortNumber)]
queryPool = do
    sock <- socket AF_INET Datagram defaultProtocol

    let returnAddr = (SockAddrInet queryResponsePort (tupleToHostAddress (0, 0, 0, 0)))
    bind sock returnAddr

    (sendPoolMsg . show) Ping
    responses <- gatherInput queryTimeout (recvFrom sock 100)
    
    close sock

    (pure . catMaybes . map extractResponse) responses
  where
    extractResponse :: (ByteString, SockAddr) -> Maybe (Name, HostAddress, PortNumber)
    extractResponse (msg, (SockAddrInet _ ip)) =
      case (read . unpack) msg :: Response of
        Pong name port -> Just (name, ip, port)
        _              -> Nothing
    extractResponse _ = Nothing

listenForPingUnsafe :: IO (Maybe HostAddress)
listenForPingUnsafe = do
  debugM
    logID
    (concat ["Trying to recv from ", show poolGrp, " on ", show listenPort])

  bracket
    (multicastReceiver poolGrp listenPort)
    close
    listenForPingUnsafe'

respondToPingUnsafe :: Name -> HostAddress -> IO ()
respondToPingUnsafe name remoteIp = do
  bracket
    (socket AF_INET Datagram defaultProtocol)
    close
    (\sock -> answerPingUnsafe sock name 4000 remoteIp)

listenToPoolUnsafe :: Name -> IO()
listenToPoolUnsafe name = do
  maybeRemoteIp <- listenForPingUnsafe

  case maybeRemoteIp of
    Just remoteIp -> respondToPingUnsafe name remoteIp
    _ -> pure ()

listenForPingUnsafe' :: Socket -> IO (Maybe HostAddress)
listenForPingUnsafe' sock = do
  debugM
    logID
    "Listening for udp pings..."

  (msg, addr@(SockAddrInet _ remoteIp)) <- recvFrom sock 100

  debugM
    logID
    (concat ["Received ", show msg, " from ", show addr])
  
  let maybeRequest = (readMaybe . unpack) msg :: Maybe Request

  pure $ case maybeRequest of
    Just Ping -> Just remoteIp
    _ -> Nothing

answerPingUnsafe :: Socket -> Name -> PortNumber -> HostAddress -> IO ()
answerPingUnsafe socket name tcpPort remoteIp = do
  let responseAddr = (SockAddrInet queryResponsePort remoteIp)

  debugM
    logID
    (concat ["Sending udp ping answer to ", show responseAddr])

  void (
      sendTo 
        socket
        ((pack . show) (Pong name tcpPort))
        responseAddr
    )

pingListenerServiceUnsafe :: Name -> PortNumber -> IO()
pingListenerServiceUnsafe name tcpPort = do
  bracket
    (multicastReceiver poolGrp listenPort)
    close
    (\sock ->
        (sequence_ . repeat)
          (do
              maybeAddr <- listenForPingUnsafe' sock

              maybe
                (pure ())
                (answerPingUnsafe sock name tcpPort)
                maybeAddr
            )
      )
  
