module Main where

import Chat (initPeer, logID)
import Discovery (logID)
import MulticastDiscovery (logID)
import BroadcastDiscovery (logID)
import IrcDiscovery (logID)
import Connections (logID)
import TaskManager (logID)
import IODrivers (driveStdin, driveFileInput, logID)

import System.Log.Logger (
      updateGlobalLogger
    , setLevel
    , Priority(DEBUG)
    , debugM
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
    , optional
    , argument
    , short
    , long
    , help
    , switch
    , execParser
    , strOption
  )

import Control.Applicative ((<**>))
import Control.Monad (when)

import Data.Maybe (maybe)

import Utils (genPeerId)

-- Datatype which will encode the cli options the program
-- was started with
-- 
-- d-Flag: Indicates, whether debug log messages shall be shown
-- name-String: String, by which the chat peer shall identify itself to others
data Options
  = Options Bool (Maybe FilePath) (Maybe String)

-- Used to identify this file as source of log messages
logID :: String
logID = "Main"

options :: Parser Options
-- ^description, of how the cli options shall be parsed.
-- See Options.Applicative library
options = Options
  <$> switch ( -- describe "-d" switch option to enable debug logging
         long "debug"
      <> short 'd'
      <> help "Print debugging logs"
    )
  <*> optional (
      strOption
        (long "file"
            <> short 'f'
            <> metavar "FILE"
            <> help "Send contents of file to chat group."
          )
    )
  -- name argument, by which the chat peer shall identify itself:
  <*> optional (
        argument str (
               metavar "name"
            <> help "Custom name to identfy by. Generated automatically if not provided"
          )
      )

main :: IO ()
main = processOptions =<< execParser opts
  -- ^parse cli arguments and options and apply them using processOptions
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Start a command-line chat with others on your network. Other peers are detected automatically."
     <> header "chat - commandline p2p chat" )

processOptions :: Options -> IO ()
-- ^run the chat program and apply parsed cli options
processOptions (Options debugFlag inputFile name) = do
  -- if the debug flag is set, enable DEBUG level output
  -- for all loggers
  when
    debugFlag 
    (do
        updateGlobalLogger Discovery.logID (setLevel DEBUG)
        updateGlobalLogger MulticastDiscovery.logID (setLevel DEBUG)
        updateGlobalLogger BroadcastDiscovery.logID (setLevel DEBUG)
        updateGlobalLogger IrcDiscovery.logID (setLevel DEBUG)
        updateGlobalLogger Main.logID (setLevel DEBUG)
        updateGlobalLogger Chat.logID (setLevel DEBUG)
        updateGlobalLogger Connections.logID (setLevel DEBUG)
        updateGlobalLogger TaskManager.logID (setLevel DEBUG)
        updateGlobalLogger IODrivers.logID (setLevel DEBUG)
      )

  -- if a name is supplied on the cli, use it, otherwise,
  -- try to generate one
  identity <- maybe
    genPeerId
    pure
    name

  let {
      inputDriver = case inputFile of
        Nothing -> driveStdin
        (Just path) -> driveFileInput path
    }

  debugM
    Main.logID
    (concat ["Initializing peer as '", identity, "'..."])

  -- run the p2p chat program while identifiying with 'name' to other peers
  initPeer identity inputDriver
