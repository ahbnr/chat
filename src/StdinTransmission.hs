{-# LANGUAGE OverloadedStrings #-}
module StdinTransmission where

import Conduit
import Control.Concurrent.Async (race)
import Data.Conduit.Network

import Data.ByteString.Char8 (pack, unpack)

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

import Control.Exception (bracket)
import Control.Monad (void)

type Address = String
type Port = Int

server :: Address -> Port -> IO()
server addr port =
    bracket
      (bracket
          (socket AF_INET Stream defaultProtocol)
          close
          (\sock -> do
              setSocketOption sock ReuseAddr 1

              -- TODO: unsafe
              (localAddrInfo:_) <- getAddrInfo Nothing (Just addr) ((Just . show) port) 
              bind sock (addrAddress localAddrInfo)
              listen sock 1
              (conn, _) <- accept sock

              pure conn
            )
        )
      close
      (\conn ->
          void $ race
              (stdinC $$ sinkSocket conn)
              (sourceSocket conn $$ stdoutC)
        )

client :: Address -> Port -> IO()
client addr port = runTCPClient (clientSettings port (pack addr)) $ \server ->
    void $ race
        (stdinC $$ appSink server)
        (appSource server $$ stdoutC)
