{-# LANGUAGE OverloadedStrings #-}
module Connections where

import Conduit (runConduit, (.|), Void, ConduitT)
import Data.Conduit.Network (
      sourceSocket
    , sinkSocket
    , runTCPClient
    , appSource
    , appSink
    , clientSettings
  )

import Control.Concurrent (myThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (dupTMChan, TMChan)

import Data.Conduit.TMChan (sinkTMChan, sourceTMChan, isClosedTMChan)

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
    , defaultProtocol
    , SocketType(Stream)
    , Family(AF_INET)
    , Socket
    , PortNumber
  )

import System.Log.Logger (debugM)

import Control.Exception (uninterruptibleMask, finally)
import Control.Monad (void, forever, when)

import TaskManager (withTaskManager, manage, wait)
import Utils (allowCancel, onCancel)

-- Used to identify this file as source of log messages
logID :: String
logID = "Connections"

type Address = String

handleConnection :: Socket -> ConduitT () ByteString IO () -> ConduitT ByteString Void IO () -> IO()
handleConnection connection msgInC msgOutC = 
-- ^operate a connection by redirecting application input (msgInC, usually stdin) to
--  the connected peer and incoming messages to application output (msgOutC, usually
--  stdout)
  (allowCancel . finally
      (withTaskManager
          (\tm -> do
              let outgoing = runConduit $ msgInC .| sinkSocket connection
              let incoming = runConduit $ sourceSocket connection .| msgOutC

              manage tm outgoing
              manage tm incoming

              (void . wait) tm
            )
        )
    ) (close connection)

prepareServerSock :: Address -> PortNumber -> IO Socket
prepareServerSock addr port = do
-- prepare a socket on which we will listen for tcp connections
  -- create tcp socket
  sock <- socket AF_INET Stream defaultProtocol

  -- get necessary information to bind to local address
  (localAddrInfo:_) <- getAddrInfo Nothing (Just addr) ((Just . show) port) 

  -- bind socket and listen for connections
  bind sock (addrAddress localAddrInfo)
  listen sock 1 -- 1 indicates the maximum number of queued connections. (Maybe increase that in the future!)

  pure sock

server :: Socket -> TMChan ByteString -> TMChan ByteString -> IO()
server sock inputChan stdoutChan =
-- accept all incoming connections on the given socket and
-- spawn threads to handle them
  (allowCancel . withTaskManager)
    (\serverTm ->
        (finally
            (forever $ do -- ...accept any client and start a haskell-thread for it
                -- accept incoming connection
                (connection, _) <- accept sock

                -- create clone of input channel, so that its
                -- content may be used with the connection
                -- (this way, for example stdin can be distributed
                --  to every connected peer)
                inputClone <- atomically (dupTMChan inputChan)
                
                -- spawn a thread to handle the new connection
                manage serverTm (
                    handleConnection
                      connection
                      (sourceTMChan inputClone)
                      (sinkTMChan stdoutChan)
                  )
              )
            (close sock)
          )
      )

client :: Address -> PortNumber -> TMChan ByteString -> TMChan ByteString -> IO()
client addr port msgInC msgOutC =
-- ^connect to another peer and display its messages on application output (msgOutC,
--  usually stdout) and redirect application input (msgInC, usually stdin) to it
  uninterruptibleMask (\restore -> do
  -- ^prevent async cancellation until connection is established so that we are
  --  able to flush the remaining data into the connection before shutdown.
  --  If another thread tries to shut down the application, it will be blocked,
  --  until `restore` has been called.
      threadId <- myThreadId

      debugM
        Connections.logID
        (concat ["Connecting as client (", show threadId, ") to ", show addr, ":", show port, "..."])

      runTCPClient
        (clientSettings (fromIntegral port) (pack addr))
        (\connection ->
            onCancel
              (do
                  debugM
                      Connections.logID
                      (concat ["Connection to ", show addr, ":", show port, " was cancelled by application shutdown.",
                                "\n", "Trying to flush input into connection before terminating, if the corresponding channel was closed."])

                  closed <- atomically (isClosedTMChan msgInC)
                    
                  when
                    closed
                    (runConduit $ sourceTMChan msgInC .| appSink connection)
                )
              (restore
              -- ^reenable async cancellation
                  (withTaskManager
                      (\tm -> do
                          manage tm ((allowCancel . runConduit) $ sourceTMChan msgInC .| appSink connection)
                          manage tm ((allowCancel . runConduit) $ appSource connection .| sinkTMChan msgOutC)

                          (void . wait) tm
                        )
                    )
                )
          )
    )
