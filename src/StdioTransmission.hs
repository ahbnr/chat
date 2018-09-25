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
  void $ race
      (runConduit $ msgInC .| sinkSocket connection)
      (runConduit $ sourceSocket connection .| msgOutC)

prepareServerSock :: Address -> PortNumber -> IO Socket
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
    (forever $ do -- ...accept any client and start a haskell-thread for it
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
client addr port msgInC msgOutC = runTCPClient (clientSettings (fromIntegral port) (pack addr)) $ \connection ->
    void $ race
        (runConduit $ msgInC .| appSink connection)
        (runConduit $ appSource connection .| msgOutC)
