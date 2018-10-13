module Utils where

import System.Posix.Process (getProcessID)
import System.Posix.User (getEffectiveUserName)
import System.Timeout (timeout)
import Network.Socket (HostAddress, hostAddressToTuple)
import Network.HostName (getHostName)

import Control.Exception (catch)
import Control.Concurrent.Async (AsyncCancelled(AsyncCancelled))

import Data.Maybe (catMaybes, isNothing)
import Data.List (intercalate)

type Microseconds = Int;

takeUntilM :: Monad m => (a -> Bool) -> m a -> m [a]
takeUntilM terminationCond action = do
-- ^execute an monadic action and apply a predicate to its result until
--  the predicate returns false. Returns a list of all results but the
--  one which failed the predicate.
  -- execute action
  result <- action
  -- check result
  if terminationCond result then
    -- terminate, if predicate is negative
    pure []
  else do
    -- otherwise continue executing the action...
    recursion <- takeUntilM terminationCond action
    -- ... and collect the results
    pure (result:recursion)

gatherInput :: Microseconds -> IO a -> IO [a]
gatherInput timeoutMillisecs =
-- ^execute an io action repeatedly for a fixed ammount of time
--  and return list of results
    fmap catMaybes
  . takeUntilM isNothing
  . timeout timeoutMillisecs

addrToString :: HostAddress -> String
addrToString addr = 
-- ^converts an ipv4 into string representation
  let (a, b, c, d) = hostAddressToTuple addr in
    intercalate
      "."
      [show a, show b, show c, show d]

genPeerId :: IO String
genPeerId = do
-- ^generate an id to be used to identify ourself to others
  pid <- getProcessID
  user <- getEffectiveUserName
  host <- getHostName

  -- to generate a somewhat "unique" id on the network
  -- and on the local machine,we combine the process pid,
  -- the current user and the hostname
  (pure . concat) [show pid, "@", user, "@", host]

onCancel :: IO a -> IO a -> IO a
-- ^On AsyncCancelled while exectuting the given task, run an specific action.
-- It does not rethrow AsyncCancelled.
onCancel action task =
  catch
    task
    (\AsyncCancelled -> action)

allowCancel :: IO () -> IO ()
-- ^Ignore AsyncCancelled exceptions, which indicate proper shutdown
allowCancel = onCancel (pure ())
