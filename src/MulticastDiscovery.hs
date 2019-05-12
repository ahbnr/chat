module MulticastDiscovery where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import Control.Exception (bracket)
import Control.Monad (void)

import Data.Maybe (maybe, mapMaybe, maybeToList)
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
import System.Timeout (timeout)

import Numeric.Natural as Natural

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
queryTimeout = 1200000

-- Maximum number of retries for searching for other peers
poolQueryingRetries :: Natural
poolQueryingRetries = 2

recvMax :: Int
recvMax = 4096

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
    retryNTimes poolQueryingRetries
    -- ^retry, if no peers have been found
      (bracket
          (socket AF_INET Datagram defaultProtocol)
          close
          (\sock -> do
              -- address on which we will listen for responses to our
              -- query
              let returnAddr = SockAddrInet queryResponsePort (tupleToHostAddress (0, 0, 0, 0))

              bind sock returnAddr

              -- ping all peers (multicast)
              (sendPoolMsg . show) Ping

              -- wait for the first response with twice the timeout
              firstResponse <- timeout (2*queryTimeout) (recvFrom sock recvMax)
              -- collect more responses within a fixed timeout
              additionalResponses <- gatherInput queryTimeout (recvFrom sock recvMax)

              let responses = (maybeToList firstResponse) ++ additionalResponses

              (pure . mapMaybe extractResponse) responses
            )
        )
  where
    extractResponse :: (ByteString, SockAddr) -> Maybe (Name, HostAddress, PortNumber)
    extractResponse (msg, SockAddrInet _ ip) =
    -- ^interpret a received message and return data about sender, if valid
      (   fmap (\(Pong name port) -> (name, ip, port))
          . readMaybe
          . unpack
        ) msg
    extractResponse _ = Nothing

    retryNTimes :: Natural -> IO [a] -> IO [a]
    retryNTimes 0 action = action
    retryNTimes retries action = do
    -- ^repeat the given action, until a non empty list is returned, or the action
    --  has been retried n times already (n == `retries`)
      result <- action
      if null result then
        retryNTimes (retries - 1) action
      else
        pure result

listenForPingUnsafe :: Socket -> IO (Maybe HostAddress)
listenForPingUnsafe sock = do
-- ^listen for a ping on the given socket and return the sender address
  debugM
    logID
    "Listening for udp pings..."

  (msg, addr@(SockAddrInet _ remoteIp)) <- recvFrom sock recvMax

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
  
