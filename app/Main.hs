module Main where

import Chat (initPeer, logID)
import MulticastDiscovery (logID)

import System.Log.Logger (
      updateGlobalLogger
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

import Control.Applicative ((<**>))
import Control.Monad (when)

-- Datatype which will encode the cli options the program
-- was started with
-- 
-- d-Flag: Indicates, whether debug log messages shall be shown
-- name-String: String, by which the chat peer shall identify itself to others
data Options
  = Options Bool String

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
  -- name argument, by which the chat peer shall identify itself:
  <*> argument str (metavar "name") 

main :: IO ()
main = processOptions =<< execParser opts
  -- ^parse cli arguments and options and apply them using processOptions
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Find a peer by name and start a stdin chat. Achtung: Ranzige Alpha-Version"
     <> header "chat - commandline peer to peer chat" )

processOptions :: Options -> IO ()
-- ^run the chat program and apply parsed cli options
processOptions (Options debugFlag name) = do
  -- if the debug flag is set, enable DEBUG level output
  -- for all loggers
  when
    debugFlag 
    (do
        updateGlobalLogger MulticastDiscovery.logID (setLevel DEBUG)
        updateGlobalLogger Main.logID (setLevel DEBUG)
        updateGlobalLogger Chat.logID (setLevel DEBUG)
      )
  -- run the p2p chat program while identifiying with 'name' to other peers
  initPeer name
