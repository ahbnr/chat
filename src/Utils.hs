module Utils where

import System.Timeout
import Data.Maybe

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
