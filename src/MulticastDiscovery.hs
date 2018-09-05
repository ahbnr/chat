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
  )

import Network.Socket.ByteString (sendTo, recvFrom)
import Network.Multicast

import System.Log.Logger

import Utils (void, gatherInput, Microseconds)

type Name = String
data Request = Ping deriving (Show, Read)
data Response = Pong String deriving (Show, Read)

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

queryPool :: IO [(Name, HostAddress)]
queryPool = do
    sock <- socket AF_INET Datagram defaultProtocol

    let returnAddr = (SockAddrInet queryResponsePort (tupleToHostAddress (0, 0, 0, 0)))
    bind sock returnAddr

    (sendPoolMsg . show) Ping
    responses <- gatherInput queryTimeout (recvFrom sock 100)
    
    close sock

    (pure . catMaybes . map extractResponse) responses
  where
    extractResponse :: (ByteString, SockAddr) -> Maybe (Name, HostAddress)
    extractResponse (msg, (SockAddrInet _ ip)) =
      case (read . unpack) msg :: Response of
        Pong name -> Just (name, ip)
        _         -> Nothing
    extractResponse _ = Nothing

listenForPingUnsafe :: IO (Maybe HostAddress)
listenForPingUnsafe = do
  debugM
    logID
    (concat ["Trying to recv from ", show poolGrp, " on ", show listenPort])

  (msg, remoteIp) <- bracket
    (multicastReceiver poolGrp listenPort)
    close
    (\sock -> do
        (msg, addr@(SockAddrInet _ remoteIp)) <- recvFrom sock 100

        debugM
          logID
          (concat ["Received ", show msg, " from ", show addr])

        pure (msg, remoteIp)
      )
  
  let request = (readMaybe . unpack) msg :: Maybe Request

  pure $ case request of
    Just Ping -> Just remoteIp
    _ -> Nothing

respondToPingUnsafe :: Name -> HostAddress -> IO ()
respondToPingUnsafe name remoteIp = do
  let responseAddr = (SockAddrInet queryResponsePort remoteIp)

  debugM
    logID
    (concat ["Sending response to ", show responseAddr])

  bracket
    (socket AF_INET Datagram defaultProtocol)
    close
    (void . (\responseSock -> sendTo 
        responseSock
        ((pack . show . Pong) name)
        responseAddr
      ))

listenToPoolUnsafe :: Name -> IO()
listenToPoolUnsafe name = do
  maybeRemoteIp <- listenForPingUnsafe

  case maybeRemoteIp of
    Just remoteIp -> respondToPingUnsafe name remoteIp
    _ -> pure ()
