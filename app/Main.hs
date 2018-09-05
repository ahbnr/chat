module Main where

import Lib
import MulticastDiscovery
import StdinTransmission

import Network.Socket

import System.Log.Logger

import Options.Applicative
import Data.Semigroup ((<>))
import Data.List (intercalate, find)
import Control.Monad

data Options
  = Options Bool Command

data Command
  = Connect String
  | Listen String
  | Client String
  | Server String
  deriving Show

logID :: String
logID = "Main"

streamPort :: Int
streamPort = 4000

connectOptions :: Parser Command
connectOptions = Connect <$> argument str (metavar "name")

listenOptions :: Parser Command
listenOptions = Listen <$> argument str (metavar "name")

clientOptions :: Parser Command
clientOptions = Client <$> argument str (metavar "address")

serverOptions :: Parser Command
serverOptions = Server <$> argument str (metavar "address")

options :: Parser Options
options = Options
  <$> switch (
         long "debug"
      <> short 'd'
      <> help "Print debugging logs"
    )
  <*> subparser
    (      command "connect"
              (info connectOptions (progDesc "Chat with somebody"))
        <> command "listen"
              (info listenOptions (progDesc "Wait for incoming chats"))
        <> command "client"
              (info clientOptions (progDesc "Open a TCP connection"))
        <> command "server"
              (info serverOptions (progDesc "Wait for a TCP connection"))
      )

main :: IO ()
main = processOptions =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Find a peer by name and start a stdin chat. Achtung: Ranzige Alpha-Version"
     <> header "chat - commandline peer to peer chat" )

processOptions :: Options -> IO ()
processOptions (Options debugFlag cmd) = do
  when
    debugFlag 
    (do
        updateGlobalLogger MulticastDiscovery.logID (setLevel DEBUG)
        updateGlobalLogger Main.logID (setLevel DEBUG)
      )
  processCommand cmd

processCommand :: Command -> IO ()
processCommand (Connect name) = do
    results <- queryPool
    debugM
      Main.logID
      (concat ["Found the following peers: ", show results])

    let maybeTarget = find ((== name) . fst) results

    case maybeTarget of
      Just (name, remoteIp) -> client (addrToString remoteIp) streamPort
      _ -> errorM Main.logID ("Cant find " ++ name)
  where
    addrToString :: HostAddress -> String
    addrToString addr = 
      let (a, b, c, d) = hostAddressToTuple addr in
        intercalate
          "."
          [show a, show b, show c, show d]

processCommand (Listen name) = do
  -- updateGlobalLogger MulticastDiscovery.logID (setLevel DEBUG)
  listenToPoolUnsafe name
  debugM Main.logID "Building up connection..."
  server "0.0.0.0" streamPort

processCommand (Client addr) = client addr streamPort

processCommand (Server addr) = server addr streamPort
