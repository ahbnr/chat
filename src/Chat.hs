module Chat where

import MulticastDiscovery (pingListenerServiceUnsafe, queryPool)

import StdioTransmission (
      prepareServerSock
    , server
    , client
  )

import Utils (addrToString, allowCancel)

import TaskManager (TaskManager, manage, shutdown, wait, withTaskManager)

import Network.Socket (
      socketPort
    , HostAddress
    , PortNumber
  )

import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMChan (
      newTMChan
    , dupTMChan
    , closeTMChan
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

import System.Log.Logger (debugM, errorM)

import Control.Monad (void)

import Control.Exception (catch, throw, finally, AsyncException(UserInterrupt))

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

initServer :: TMChan ByteString -> TMChan ByteString -> TaskManager () -> IO PortNumber
-- ^creates an asynchronously running server thread using the 'server' function (see below)
-- It will listen on every address on a random free port.
-- It returns the used port and a handle for the asynchronously running server thread.
initServer stdinChan stdoutChan tm = do
  debugM Chat.logID "Listening for incoming tcp connections..."

  -- open a socket on all local addresses on a random port (indicated by 0)
  serverSock <- prepareServerSock "0.0.0.0" 0

  -- remember the used port, to use it multicast discovery later
  tcpPort <- socketPort serverSock -- TODO: unsafe!

  -- asynchonously run the server.
  -- It will send stdio to all connected clients and redirect any incoming
  -- messages to stdout.
  manage tm (
      server
        serverSock
        stdinChan
        stdoutChan
    )

  -- return the used tcp port and a handle to the server thread
  pure tcpPort

initDiscoveryService :: String -> PortNumber -> TaskManager () -> IO ()
initDiscoveryService name tcpPort tm = do
-- ^runs a thread which will answer to other peers, searching for us on the network
  debugM Chat.logID "Listening for discovery udp pings..."

  -- this asynchronously launched background service will answer
  -- discovery requests from other peers
  manage tm (
      pingListenerServiceUnsafe name tcpPort
    )

connectToPeers :: String -> TMChan ByteString -> TMChan ByteString -> TaskManager () -> [(String, HostAddress, PortNumber)] -> IO ()
connectToPeers ownName stdinChan stdoutChan tm =
-- ^connects to each peer in a given list by spawning client threads
      sequence_
    . map (\(_, remoteIp, tcpPort) -> do
              debugM
                Chat.logID
                (concat ["Connecting to ", show remoteIp])

              -- duplicate stdin to be redirected to the current client
              clientStdinChan <- atomically (dupTMChan stdinChan)

              -- this client thread will connect to the peer,
              -- redirect stdin to it and its messages to our stdout
              manage tm (client
                  (addrToString remoteIp)
                  tcpPort
                  clientStdinChan
                  stdoutChan
                )
            )
    . filter (\(remoteName, _, _) -> remoteName /= ownName) -- dont connect to yourself
  
initStdioDrivers :: TMChan ByteString -> TMChan ByteString -> TaskManager () -> IO ()
initStdioDrivers stdinChan stdoutChan tm = do
-- ^run threads, which will handle redirecting stdout and stdin from and to threads
--  by using channels
    manage tm stdinDriver
    manage tm stdoutDriver
  where
    -- redirect stdin to the stdin stm channel, which will be used to distribute
    -- stdin to all threads which need its content
    stdinDriver = allowCancel $ do
      debugM
        Chat.logID
        "Enabling redirection of stdin into established connections..."

      -- if stdinC ever closes (EOF), also close the forwarding channel
      finally
        (runConduit $ stdinC .| sinkTMChan stdinChan)
        (atomically $ closeTMChan stdinChan)

      debugM
        Chat.logID
        "Stdin stopped (usually cause of EOF). Shutting down all threads..."

      shutdown tm

      debugM
        Chat.logID
        "Application shut down by stdin driver."

    -- redirect everything send to the stm stdout-Channel to stdout
    stdoutDriver = allowCancel $ do
      debugM
        Chat.logID
        "Enabling redirection of arriving data from connections into stdout..."

      runConduit (sourceTMChan stdoutChan .| stdoutC)
    
      errorM
        Chat.logID
        "Stdout source channel was closed (this should not happen)"
  
initPeer :: String -> IO ()
-- ^initialize all procedures required to boot a fully functional chat peer
initPeer name = do
  (stdinChan, stdoutChan) <- atomically prepareStdioChannels

  withTaskManager
    (\tm -> do
        tcpPort <- initServer stdinChan stdoutChan tm

        initDiscoveryService name tcpPort tm

        -- search for other peers, so that we may connect to them
        peers <- queryPool
        debugM
          Chat.logID
          (concat ["Found the following peers: ", show peers])


        debugM
          Chat.logID
          "Trying to connect to each discovered peer..."

        -- connect to each found peer
        connectToPeers name stdinChan stdoutChan tm peers

        -- Enable stdio
        initStdioDrivers stdinChan stdoutChan tm

        -- wait for all threads to finish
        -- TODO: Implement proper shutdown procedure
        catch
          ((void . wait) tm)
          (\e ->
              case e of
                UserInterrupt -> (do
                    debugM
                      Chat.logID
                      "Main thread interrupted by user (probably Ctrl-C). Shutting down..."
                  )
                _ -> throw e
              )
      )
