module Utils where

import System.Timeout (timeout)
import Network.Socket (HostAddress, hostAddressToTuple)

import Data.Maybe (catMaybes, isNothing)
import Data.List (intercalate)

type Microseconds = Int;

void :: IO a -> IO ()
void action = do
  _ <- action
  pure ()

takeUntilM :: Monad m => (a -> Bool) -> m a -> m [a]
takeUntilM terminationCond action = do
  result <- action
  if terminationCond result then
    pure []
  else do
    recursion <- takeUntilM terminationCond action
    pure (result:recursion)

gatherInput :: Microseconds -> IO a -> IO [a]
gatherInput timeoutMillisecs =
    fmap catMaybes
  . takeUntilM isNothing
  . timeout timeoutMillisecs

addrToString :: HostAddress -> String
addrToString addr = 
  let (a, b, c, d) = hostAddressToTuple addr in
    intercalate
      "."
      [show a, show b, show c, show d]
