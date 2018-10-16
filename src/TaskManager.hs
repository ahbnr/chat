module TaskManager where

import Control.Exception (Exception, throw, bracket, throwTo)

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (Async, wait, cancel, async, asyncThreadId, AsyncCancelled(AsyncCancelled))
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar)

import Control.Monad (unless, when, void)
import Control.Monad.STM (STM, atomically)

import System.Log.Logger (debugM)

import Data.Typeable (Typeable)

-- Used to identify this file as source of log messages
logID :: String
logID = "TaskManager"

-- What shall TaskManager accomplish?
--
-- * tasks shall be able to shut down other managed tasks,
--   when given the instance of their own TaskManager.
--   This way, critical threads, like the stdin driver thread, are able to shutdown the
--   application. For example, stdin should cause this on EOF.
-- * the main thread should be to wait for completion of threads
-- * the main thread should be able to shut down all threads, for example on Ctrl-C
-- * multiple threads trying to shutdown the application should be handled correctly
--   (for example, if stdin and stdout drivers fail/complete at the same time)
-- * trying to add threads after a shutdown should be handled by
--   immediately canceling the new thread
--   (this is necessary, if a crucial thread fails early)

newtype TaskManager a = TaskManager (TVar (
      [Async a] -- ^all managed threads
    , Bool -- ^'locked', whether the TaskManager is locked, and cant accept any more threads
    , Bool -- ^'isShutDown', whether the TaskManager is shutting down or is already shut down
  ))

data TaskManagerException = ManagerWaiting
    deriving Typeable
instance Show TaskManagerException where
    show ManagerWaiting = "No more threads can be managed by this TaskManager instance, since it is waiting for completion of all its threads."
instance Exception TaskManagerException

withTaskManager :: (TaskManager a -> IO b) -> IO b
-- ^create a taskmanager instance to be used in the given action.
-- If the action terminates or an exception is thrown, all tasks
-- in the taskmanager will be shut down and it will wait for all
-- tasks to complete.
withTaskManager =
    bracket
      (atomically mkTaskManager)
      (\tm -> do
          shutdown tm
          (void . TaskManager.wait) tm
        )
  where
    mkTaskManager :: STM (TaskManager a)
    mkTaskManager = do
      status <- newTVar ([], False, False)
      (pure . TaskManager) status

manage :: TaskManager a -> IO a -> IO ()
-- ^add a task to the task manager.
-- If the task manager is shutting down or already did, the new thread will be canceled
-- after starting and this call blocks until it completed.
-- If another thread is waiting for all managed tasks to complete but no shutdown
-- is in progress, this call will fail with ManagerWaiting :: TaskManagerException
manage (TaskManager status) action = do
  task <- async action

  debugM
    TaskManager.logID
    (concat ["Preparing to manage thread with id ", (show . asyncThreadId) task])

  -- atomically check the taskmanager status and update it if necessary
  (locked, isShutDown) <- atomically (do
      -- get status values
      (tasks, locked, isShutDown) <- readTVar status

      -- if the manager is neither locked nor shut down, the list of
      -- managed tasks is updated
      when
        (not locked && not isShutDown)
        (writeTVar
            status
            (task:tasks, locked, isShutDown)
          )

      -- return lock and shutdown status
      pure (locked, isShutDown)
    )

  if isShutDown then
    cancel task
  else
    when
      locked
      -- locked but not shut down means, a process is waiting on all managed threads to complete.
      -- In that case, no more threads may be added, therefore an exception is thrown.
      (throw ManagerWaiting) 

shutdown :: TaskManager a -> IO ()
-- ^will instruct all threads managed to shut down by sending async exceptions to them.
-- Exceptions are only sent, if this is the first call to shutdown.
-- It will *not* block, use `wait` for that.
-- After calling, no additional threads can be managed.
-- If a thread within the managed group calls shutdown, it will not be canceled.
shutdown (TaskManager status) = do
    -- atomically query the manager's status and set the 'shut down' flag, if it wasnt shut down already
    -- Also retrieve the original shutdown status and managed tasks.
    (tasks, isShutDown) <- atomically (do
        (tasks, _, isShutDown) <- readTVar status

        unless
          isShutDown
          (writeTVar
              status
              (    tasks
                 , True -- mark as locked and shut down
                 , True
               )
            )

        pure (tasks, isShutDown)
      )

    callingID <- myThreadId

    debugM
      TaskManager.logID
      (concat ["Shutting down managed threads (caused by id ", show callingID, ")"])

    if isShutDown then
      debugM
        TaskManager.logID
        (concat ["This TaskManager is already shutting down. Continueing."])
    else (do
        (
            mapM_ shutdownAction
          . filter ((callingID /=) . asyncThreadId)
          ) tasks

        debugM
          TaskManager.logID
          (concat ["Cancelled threads (", show callingID, ")"])
      )
  where
    shutdownAction :: Async a -> IO ()
    shutdownAction task = do
      debugM
        TaskManager.logID
        (concat ["Canceling thread with id ", (show . asyncThreadId) task])

      asyncCancel task

    asyncCancel :: Async a -> IO()
    -- ^cancel a thread, but do not wait for it.
    asyncCancel task = throwTo (asyncThreadId task) AsyncCancelled
  
wait :: TaskManager a -> IO [a]
-- ^wait for all tasks managed to be completed by stopping execution until this
-- condition is reached.
-- After calling, no additional threads can be managed.
-- The calling thread will not wait for itself, if it is beeing managed.
-- Be careful, not to let threads wait for each other (deadlock).
wait (TaskManager status) = do
  -- atomically query for managed tasks, and set status to locked, if not set already.
  -- This way, no more tasks can be added, while waiting.
  tasks <- atomically (do
      (tasks, locked, isShutDown) <- readTVar status

      unless
        locked
        (writeTVar
            status
            (tasks, True, isShutDown)
          )

      pure tasks
    )

  -- wait for tasks, but dont wait for youself, if you're a managed task
  callingID <- myThreadId

  (
       mapM Control.Concurrent.Async.wait
     . filter ((callingID /=) . asyncThreadId)
    ) tasks
