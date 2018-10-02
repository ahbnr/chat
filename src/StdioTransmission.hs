{-# LANGUAGE OverloadedStrings #-}
module StdioTransmission where

import Conduit (runConduit, (.|), Void, ConduitT)
import Data.Conduit.Network (
      sourceSocket
    , sinkSocket
    , runTCPClient
    , appSource
    , appSink
    , clientSettings
  )

import Control.Concurrent.Async (race)

import Network.Socket (Socket, PortNumber)

import GHC.Conc (atomically)
import Control.Concurrent.STM.TMChan (dupTMChan, TMChan)
import Data.Conduit.TMChan (sinkTMChan, sourceTMChan)

import Control.Concurrent.Async (async)

import Data.ByteString.Char8 (
      pack
    , ByteString
  )

import Network.Socket (
      socket
    , close
    , bind
    , accept
    , listen
    , getAddrInfo
    , addrAddress
    , setSocketOption
    , SocketOption(ReuseAddr)
    , defaultProtocol
    , SocketType(Stream)
    , Family(AF_INET)
  )

import Control.Exception (finally)
import Control.Monad (void, forever)

type Address = String

-- type MsgInC = MonadIO m => ConduitT i ByteString m ()
-- type MsgOutC = MonadIO m => ConduitT ByteString o m ()

handleConnection :: Socket -> ConduitT () ByteString IO () -> ConduitT ByteString Void IO () -> IO()
handleConnection connection msgInC msgOutC = 
-- ^run a connection by redirecting application input (msgInC, usually stdin) to
--  the connected peer and incoming messages to application output (msgOutC, usually
--  stdout)
-- TODO rephrase "run"
  void $ race
      (runConduit $ msgInC .| sinkSocket connection)
      (runConduit $ sourceSocket connection .| msgOutC)

prepareServerSock :: Address -> PortNumber -> IO Socket
prepareServerSock addr port = do
-- prepare a socket on which we will listen for tco connections
  -- create tcp socket
  sock <- socket AF_INET Stream defaultProtocol

  -- TODO
  setSocketOption sock ReuseAddr 1

  -- get necessary information to bind to local address
  -- TODO: unsafe
  (localAddrInfo:_) <- getAddrInfo Nothing (Just addr) ((Just . show) port) 

  -- bind socket and listen for connections (TODO: What does "1" mean)
  bind sock (addrAddress localAddrInfo)
  listen sock 1

  pure sock

server :: Socket -> TMChan ByteString -> TMChan ByteString -> IO()
server sock stdinChan stdoutChan =
-- accept all incoming connections on the given socket and
-- spawn threads to handle them
  finally
    (forever $ do -- ...accept any client and start a haskell-thread for it
        -- accept incoming connection
        (connection, _) <- accept sock

        -- create clone of stdin channel, so that stdin
        -- content may be used with the connection
        stdinClone <- atomically (dupTMChan stdinChan)
        
        -- spawn a thread to handle the new connection
        async (
            handleConnection
              connection
              (sourceTMChan stdinClone)
              (sinkTMChan stdoutChan)
          )

        -- TODO: collect thread handles and wait for them to finish,
        -- if the server is cancelled
      )
    (close sock)

client :: Address -> PortNumber -> ConduitT () ByteString IO () -> ConduitT ByteString Void IO () -> IO()
client addr port msgInC msgOutC =
-- ^connect to another peer and display its messages on application output (msgOutC,
--  usually stdout) and redirect application input (msgInC, usually stdin) to it
  runTCPClient
    (clientSettings (fromIntegral port) (pack addr))
    (\connection ->
        void $ race
          (runConduit $ msgInC .| appSink connection)
          (runConduit $ appSource connection .| msgOutC)
      )
