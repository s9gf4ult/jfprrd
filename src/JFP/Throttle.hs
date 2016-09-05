module JFP.Throttle where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Time
import Data.Time.Clock.POSIX

makeThrottler
  :: NominalDiffTime
  -> (a -> IO ())
  -> IO (a -> IO ())
makeThrottler tdiff handler = do
  msgVar <- newTVarIO Nothing
  let epoch = posixSecondsToUTCTime 0
  sendVar <- newTVarIO epoch
  handledVar <- newTVarIO epoch
  let
    worker = forever $ do
      work <- atomically $ do
        readTVar msgVar >>= \case
          Nothing -> return $ return ()
          Just msg -> do
            lastSend <- readTVar sendVar
            lastHandled <- readTVar handledVar
            if lastSend > lastHandled
              then return $ do
                handler msg
                atomically $ writeTVar handledVar lastSend
              else return $ return ()
      work
      threadDelay $ round $ (toRational tdiff) * 1e6
  _ <- forkIO worker
  return $ \a -> do
    now <- getCurrentTime
    atomically $ do
      writeTVar msgVar $ Just a
      writeTVar sendVar now
