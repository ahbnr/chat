module IODrivers where

import Conduit (
      stdinC
    , stdoutC
    , sourceFile
    , (.|)
    , runConduitRes
    , runConduit
    , ConduitT
    , ResourceT
  )

import Data.Conduit.TMChan (sinkTMChan, sourceTMChan)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan)

import Control.Exception (finally)

import Data.ByteString.Char8 (ByteString)

import System.Log.Logger (debugM, errorM)

import TaskManager (TaskManager, manage, shutdown)
import Utils (allowCancel)

-- Used to identify this file as source of log messages
logID :: String
logID = "IODrivers"

driveInput :: ConduitT () ByteString (ResourceT IO) () -> TMChan ByteString -> TaskManager () -> IO ()
driveInput sourceC inputChan tm = allowCancel (do
    debugM
      IODrivers.logID
      "Enabling redirection of input into established connections..."

    finally
      (runConduitRes $ sourceC .| sinkTMChan inputChan)
      (do
          debugM
            IODrivers.logID
            "Input stopped (usually because of EOF). Shutting down all threads..."

          atomically (closeTMChan inputChan)

          shutdown tm

          debugM
            IODrivers.logID
            "Application shut down by input driver."
        )
  )

driveFileInput :: FilePath -> TMChan ByteString -> TaskManager () -> IO ()
-- ^redirect the contents of a file to the input stm channel, which will be used to distribute
-- the file to all threads which need its content
driveFileInput path = driveInput (sourceFile path)

driveStdin :: TMChan ByteString -> TaskManager () -> IO()
-- ^redirect stdin to the input stm channel, which will be used to distribute
-- stdin to all threads which need its content
driveStdin = driveInput (stdinC :: ConduitT i ByteString (ResourceT IO) ())
  
initIODrivers :: IO () -> TMChan ByteString -> TaskManager () -> IO ()
initIODrivers inputDriver stdoutChan tm = do
-- ^run threads, which will handle redirecting stdout and input (stdin / file) from and to threads
--  by using channels
    manage tm inputDriver
    manage tm stdoutDriver
  where
    -- redirect everything send to the stm stdout-Channel to stdout
    stdoutDriver = allowCancel $ do
      debugM
        IODrivers.logID
        "Enabling redirection of arriving data from connections into stdout..."

      runConduit (sourceTMChan stdoutChan .| stdoutC)
    
      errorM
        IODrivers.logID
        "Stdout source channel was closed (this should not happen)"
