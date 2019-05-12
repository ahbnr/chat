module Discovery where

import System.Log.Logger (debugM)

import Data.List (nub)
import Data.Function ((&))
import Control.Monad (void)

import Network.Socket (
      HostAddress
    , PortNumber
  )

import TaskManager (TaskManager, manage, wait, withTaskManager)

type Name = String
data Request = Ping deriving (Show, Read)
data Response = Pong String PortNumber deriving (Show, Read)

type Peer = (Name, HostAddress, PortNumber)

type DiscoverySender = IO [Peer]
type DiscoveryReceiver = Name -> PortNumber -> IO()
-- ^name of local peer -> port of local chat server -> (execution of discovery receiver)

logID :: String
logID = "Discovery"

discoverPeers :: [DiscoverySender] -> IO [Peer]
discoverPeers = fmap nub . fmap concat . sequence

makeVisible :: Name -> PortNumber -> TaskManager () -> [DiscoveryReceiver] -> IO ()
makeVisible name tcpPort tm = void . sequence . map (manage tm . (&) tcpPort . (&) name)
