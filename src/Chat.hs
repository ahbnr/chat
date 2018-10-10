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

import Control.Concurrent.Async (async, wait, Async)

waitAll = sequence . map wait

-- Used to identify this file as source of log messages
logID :: String
logID = "Chat"

prepareStdioChannels :: STM (TMChan ByteString, TMChan ByteString)
-- ^create stm channels to be used for distributing stdin to multiple threads
-- and and letting those threads produce output for stdout
prepareStdioChannels = do
  -- the stdin channel will be a broadcast channel, so that everything
  -- written to it, will be sent to every channel created from it
  -- by dupTMChan
  stdinChan <- newBroadcastTMChan
  stdoutChan <- newTMChan

  pure (stdinChan, stdoutChan)

initServer :: TMChan ByteString -> TMChan ByteString -> IO(PortNumber, Async ())
-- ^creates an asynchronously running server thread using the 'server' function (see below)
-- It will listen on every address on a random free port.
-- It returns the used port and a handle for the asynchronously running server thread.
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

  -- return the used tcp port and a handle to the server thread
  pure (tcpPort, streamServer)

initDiscoveryService :: String -> PortNumber -> IO (Async ())
initDiscoveryService name tcpPort = do
-- ^runs a thread which will answer to other peers, searching for us on the network
  debugM Chat.logID "Listening for discovery udp pings..."

  -- this asynchronously launched background service will answer
  -- discovery requests from other peers
  pingService <- async (pingListenerServiceUnsafe name tcpPort)

  pure pingService

connectToPeers :: String -> TMChan ByteString -> TMChan ByteString -> [(String, HostAddress, PortNumber)] -> IO [Async ()]
connectToPeers ownName stdinChan stdoutChan =
-- ^connects to each peer in a given list by spawning client threads
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
-- ^run threads, which will handle redirecting stdout and stdin from and to threads
--  by using channels
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
-- ^initialize all procedures required to boot a fully functional chat peer
  (stdinChan, stdoutChan) <- atomically prepareStdioChannels

  (tcpPort, streamServer) <- initServer stdinChan stdoutChan

  discoveryService <- initDiscoveryService name tcpPort

  -- search for other peers, so that we may connect to them
  peers <- queryPool
  debugM
    Chat.logID
    (concat ["Found the following peers: ", show peers])

  -- connect to each found peer
  clientThreads <- connectToPeers name stdinChan stdoutChan peers

  -- Enable stdio
  (stdinDriver, stdoutDriver) <- initStdioDrivers stdinChan stdoutChan

  -- wait for all threads to finish
  -- TODO: Implement proper shutdown procedure
  (void . waitAll) ([streamServer, discoveryService, stdinDriver, stdoutDriver] ++ clientThreads)
