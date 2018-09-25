module Chat where

import MulticastDiscovery (pingListenerServiceUnsafe, queryPool)

import StdioTransmission (
      prepareServerSock
    , server
    , client
  )

import Utils (addrToString)

import Network.Socket (
      socketPort
    , HostAddress
    , PortNumber
  )

import GHC.Conc (atomically, STM)
import Control.Concurrent.STM.TMChan (
      newTMChan
    , dupTMChan
    , newBroadcastTMChan
    , TMChan
  )

import Conduit (
      stdinC
    , stdoutC
    , (.|)
    , runConduit
  )
import Data.Conduit.TMChan (
      sinkTMChan
    , sourceTMChan
  )

import Data.ByteString.Char8 (ByteString)

import System.Log.Logger (debugM)

import Control.Monad (void)

import Control.Concurrent.Async (async, waitAny, Async)

logID :: String
logID = "Chat"

prepareStdioChannels :: STM (TMChan ByteString, TMChan ByteString)
prepareStdioChannels = do
  stdinChan <- newBroadcastTMChan
  stdoutChan <- newTMChan

  pure (stdinChan, stdoutChan)

initServer :: TMChan ByteString -> TMChan ByteString -> IO(PortNumber, Async ())
initServer stdinChan stdoutChan = do
  debugM Chat.logID "Listening for incoming tcp connections..."

  -- open a socket on all local addresses on a random port (indicated by 0)
  serverSock <- prepareServerSock "0.0.0.0" 0

  -- remember the used port, to use it multicast discovery later
  tcpPort <- socketPort serverSock -- TODO: unsafe!

  -- asynchonously run the server.
  -- It will send stdio to all connected clients and redirect any incoming
  -- messages to stdout.
  streamServer <- async (
      server
      serverSock
      stdinChan
      stdoutChan
    )

  pure (tcpPort, streamServer)

initDiscoveryService :: String -> PortNumber -> IO (Async ())
initDiscoveryService name tcpPort = do
  debugM Chat.logID "Listening for discovery udp pings..."

  -- this asynchronously launched background service will answer
  -- discovery requests from other peers
  pingService <- async (pingListenerServiceUnsafe name tcpPort)

  pure pingService

connectToPeers :: String -> TMChan ByteString -> TMChan ByteString -> [(String, HostAddress, PortNumber)] -> IO [Async ()]
connectToPeers ownName stdinChan stdoutChan =
      sequence
    . map (\(_, remoteIp, tcpPort) -> do
              debugM
                Chat.logID
                (concat ["Connecting to ", show remoteIp])

              -- duplicate stdin to be redirected to the current client
              clientStdinChan <- atomically (dupTMChan stdinChan)

              -- this client thread will connect to the peer,
              -- redirect stdin to it and its messages to our stdout
              async (client
                  (addrToString remoteIp)
                  tcpPort
                  (sourceTMChan clientStdinChan)
                  (sinkTMChan stdoutChan)
                )
            )
    . filter (\(remoteName, _, _) -> remoteName /= ownName) -- dont connect to yourself
  
initStdioDrivers :: TMChan ByteString -> TMChan ByteString -> IO (Async (), Async ())
initStdioDrivers stdinChan stdoutChan = do
  -- redirect stdin to the stdin stm channel, which will be used to distribute
  -- stdin to all threads which need its content
  stdinDriver <- async (
      runConduit $ stdinC .| sinkTMChan stdinChan
    )

  -- redirect everything send to the stm stdout-Channel to stdout
  stdoutDriver <- async (
      runConduit $ sourceTMChan stdoutChan .| stdoutC
    )

  pure (stdinDriver, stdoutDriver)
  
initPeer :: String -> IO ()
initPeer name = do
  (stdinChan, stdoutChan) <- atomically prepareStdioChannels

  (tcpPort, streamServer) <- initServer stdinChan stdoutChan

  discoveryService <- initDiscoveryService name tcpPort

  -- search for other peers, so that we may connect to them
  peers <- queryPool
  debugM
    Chat.logID
    (concat ["Found the following peers: ", show peers])

  clientThreads <- connectToPeers name stdinChan stdoutChan peers

  -- Enable stdio
  (stdinDriver, stdoutDriver) <- initStdioDrivers stdinChan stdoutChan

  (void . waitAny) ([streamServer, discoveryService, stdinDriver, stdoutDriver] ++ clientThreads)
