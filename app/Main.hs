module Main where

import MulticastDiscovery (
      pingListenerServiceUnsafe
    , queryPool
    , logID
  )

import StdioTransmission (
      prepareServerSock
    , server
    , client
  )

import Network.Socket (
      socketPort
    , HostAddress
    , hostAddressToTuple
  )

import GHC.Conc (atomically)
import Control.Concurrent.STM.TMChan (
      newTMChanIO
    , dupTMChan
    , newBroadcastTMChanIO
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

import Data.ByteString.Char8 (
      pack
    , unpack
    , ByteString
  )

import System.Log.Logger (
      debugM
    , updateGlobalLogger
    , setLevel
    , Priority(DEBUG)
  )

import Options.Applicative (
      Parser
    , metavar
    , header
    , progDesc
    , fullDesc
    , helper
    , info
    , str
    , argument
    , short
    , long
    , help
    , switch
    , execParser
  )
import Data.Semigroup ((<>))
import Data.List (intercalate, find)

import Control.Monad (when, void)
import Control.Applicative ((<**>))

import Control.Concurrent.Async (async, waitAny)

data Options
  = Options Bool String

logID :: String
logID = "Main"

options :: Parser Options
options = Options
  <$> switch (
         long "debug"
      <> short 'd'
      <> help "Print debugging logs"
    )
  <*> argument str (metavar "name")

main :: IO ()
main = processOptions =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Find a peer by name and start a stdin chat. Achtung: Ranzige Alpha-Version"
     <> header "chat - commandline peer to peer chat" )

processOptions :: Options -> IO ()
processOptions (Options debugFlag name) = do
  when
    debugFlag 
    (do
        updateGlobalLogger MulticastDiscovery.logID (setLevel DEBUG)
        updateGlobalLogger Main.logID (setLevel DEBUG)
      )
  initPeer name

initPeer :: String -> IO ()
initPeer name = do
    -- preparing stdio channels
    stdinChan <- newBroadcastTMChanIO
    stdoutChan <- newTMChanIO

    debugM Main.logID "Listening for incoming tcp connections..."

    serverSock <- prepareServerSock "0.0.0.0" 0
    tcpSock <- socketPort serverSock -- unsafe!

    streamServer <- async (
        server
        serverSock
        stdinChan
        stdoutChan
      )

    debugM Main.logID "Listening for discovery udp pings..."
    pingService <- async (pingListenerServiceUnsafe name tcpSock)

    results <- queryPool
    debugM
      Main.logID
      (concat ["Found the following peers: ", show results])

    debugM
      Main.logID
      "Trying to connect to all found peers..."

    (     sequence_
        . map (\(_, remoteIp, tcpPort) -> do
                  debugM
                    Main.logID
                    (concat ["Connecting to ", show remoteIp])

                  clientStdinChan <- atomically (dupTMChan stdinChan)

                  -- TODO wait for the client later!
                  (void . async) (client
                      (addrToString remoteIp)
                      tcpPort
                      (sourceTMChan clientStdinChan)
                      (sinkTMChan stdoutChan)
                    )
                )
        . filter (\(remoteName, _, _) -> remoteName /= name) -- dont connect to yourself
      ) results

    -- Enable stdio
    async (
        runConduit $ stdinC .| sinkTMChan stdinChan
      )

    async (
        runConduit $ sourceTMChan stdoutChan .| stdoutC
      )

    (void . waitAny) [streamServer, pingService]
  where
    addrToString :: HostAddress -> String
    addrToString addr = 
      let (a, b, c, d) = hostAddressToTuple addr in
        intercalate
          "."
          [show a, show b, show c, show d]
