module Chat where

import qualified MulticastDiscovery
import qualified BroadcastDiscovery

import Connections (
      prepareServerSock
    , server
    , client
  )

import Utils (addrToString, onUserInterrupt)

import TaskManager (TaskManager, manage, wait, withTaskManager)

import Discovery (discoverPeers, makeVisible)

import IODrivers (initIODrivers)

import Network.Socket (
      socketPort
    , HostAddress
    , PortNumber
  )

import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMChan (
      newTMChan
    , dupTMChan
    , newBroadcastTMChan
    , TMChan
  )

import Data.ByteString.Char8 (ByteString)

import System.Log.Logger (debugM)

import Control.Monad (void)

-- Used to identify this file as source of log messages
logID :: String
logID = "Chat"

prepareIOChannels :: STM (TMChan ByteString, TMChan ByteString)
-- ^create stm channels to be used for distributing input (stdin or file contents) to multiple threads
-- and and letting those threads produce output for stdout
prepareIOChannels = do
  -- the input channel will be a broadcast channel, so that everything
  -- written to it, will be sent to every channel created from it
  -- by dupTMChan
  inputChan <- newBroadcastTMChan
  stdoutChan <- newTMChan

  pure (inputChan, stdoutChan)

initServer :: TMChan ByteString -> TMChan ByteString -> TaskManager () -> IO PortNumber
-- ^creates an asynchronously running server thread using the 'server' function (see below)
-- It will listen on every address on a random free port.
-- It returns the used port and a handle for the asynchronously running server thread.
initServer inputChan stdoutChan tm = do
  debugM Chat.logID "Listening for incoming tcp connections..."

  -- open a socket on all local addresses on a random port (indicated by 0)
  serverSock <- prepareServerSock "0.0.0.0" 0

  -- remember the used port, to use it multicast discovery later
  tcpPort <- socketPort serverSock

  -- asynchonously run the server.
  -- It will send input (stdin / file) to all connected clients and redirect any incoming
  -- messages to stdout.
  manage tm (
      server
        serverSock
        inputChan
        stdoutChan
    )

  -- return the used tcp port and a handle to the server thread
  pure tcpPort

initDiscoveryServices :: String -> PortNumber -> TaskManager () -> IO ()
initDiscoveryServices name tcpPort tm = do
-- ^runs a thread which will answer to other peers, searching for us on the network
  debugM Chat.logID "Listening for discovery udp pings..."

  -- these asynchronously launched background services will answer
  -- discovery requests from other peers
  makeVisible name tcpPort tm [MulticastDiscovery.receiver, BroadcastDiscovery.receiver]

connectToPeers :: String -> TMChan ByteString -> TMChan ByteString -> TaskManager () -> [(String, HostAddress, PortNumber)] -> IO ()
connectToPeers ownName inputChan stdoutChan tm =
-- ^connects to each peer in a given list by spawning client threads
      mapM_ (\(_, remoteIp, tcpPort) -> do
              debugM
                Chat.logID
                (concat ["Connecting to ", show remoteIp])

              -- duplicate input (stdin or file) to be redirected to the current client
              clientStdinChan <- atomically (dupTMChan inputChan)

              -- this client thread will connect to the peer,
              -- redirect input to it and its messages to our stdout
              manage tm (client
                  (addrToString remoteIp)
                  tcpPort
                  clientStdinChan
                  stdoutChan
                )
            )
    . filter (\(remoteName, _, _) -> remoteName /= ownName) -- dont connect to yourself
 
initPeer :: String -> (TMChan ByteString -> TaskManager () -> IO ()) -> IO ()
-- ^initialize all procedures required to boot a fully functional chat peer
initPeer name inputDriver =
  onUserInterrupt
    (   -- this UserInterrupt handler ensures application does not exit with error code on Ctrl-C
        -- and also prints a debug message, if the debug log is enabled.
        -- Use of "withTaskManager" down below ensures, that the application is shut down
        -- properly on Ctrl-C
        debugM
          Chat.logID
          "Main thread interrupted by user (probably Ctrl-C). Shutting down..."
      )
    (do
        (inputChan, stdoutChan) <- atomically prepareIOChannels

        withTaskManager (\tm -> do
              tcpPort <- initServer inputChan stdoutChan tm

              initDiscoveryServices name tcpPort tm

              -- search for other peers, so that we may connect to them
              peers <- discoverPeers [MulticastDiscovery.sender, BroadcastDiscovery.sender]
              debugM
                Chat.logID
                (concat ["Found the following peers: ", show peers])


              debugM
                Chat.logID
                "Trying to connect to each discovered peer..."

              -- connect to each found peer
              connectToPeers name inputChan stdoutChan tm peers

              -- Enable IO by running threads that redirect
              -- incoming data into stdout and outgoing data
              -- into stm channels (which will be used to
              -- feed connections)
              initIODrivers 
                (inputDriver inputChan tm)
                stdoutChan
                tm

              -- wait for all threads to finish
              (void . wait) tm
          )
      )
