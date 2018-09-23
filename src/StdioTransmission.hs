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

import Control.Concurrent.Async (async, waitAny)

import Data.ByteString.Char8 (
      pack
    , unpack
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
    , SockAddr(SockAddrInet)
    , defaultProtocol
    , SocketType(Stream)
    , Family(AF_INET)
    , tupleToHostAddress
  )

import Control.Exception (bracket, finally)
import Control.Monad (void, forever)

type Address = String
type Port = Int

-- type MsgInC = MonadIO m => ConduitT i ByteString m ()
-- type MsgOutC = MonadIO m => ConduitT ByteString o m ()

-- TODO: Replace client function with this one
handleConnection :: Socket -> ConduitT () ByteString IO () -> ConduitT ByteString Void IO () -> IO()
handleConnection connection msgInC msgOutC = 
  void $ race
      (runConduit $ msgInC .| sinkSocket connection)
      (runConduit $ sourceSocket connection .| msgOutC)

prepareServerSock :: Address -> Port -> IO Socket
prepareServerSock addr port = do
  sock <- socket AF_INET Stream defaultProtocol

  setSocketOption sock ReuseAddr 1

  -- TODO: unsafe
  (localAddrInfo:_) <- getAddrInfo Nothing (Just addr) ((Just . show) port) 
  bind sock (addrAddress localAddrInfo)
  listen sock 1

  pure sock

server :: Socket -> TMChan ByteString -> TMChan ByteString -> IO()
server sock stdinChan stdoutChan =
  finally
    (forever $ do
        (connection, _) <- accept sock

        stdinClone <- atomically (dupTMChan stdinChan)
        
        async (
            handleConnection
              connection
              (sourceTMChan stdinClone)
              (sinkTMChan stdoutChan)
          )
      )
    (close sock)


client :: Address -> PortNumber -> ConduitT () ByteString IO () -> ConduitT ByteString Void IO () -> IO()
client addr port msgInC msgOutC = runTCPClient (clientSettings (fromIntegral port) (pack addr)) $ \server ->
    void $ race
        (runConduit $ msgInC .| appSink server)
        (runConduit $ appSource server .| msgOutC)
