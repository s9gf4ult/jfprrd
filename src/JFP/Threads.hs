module JFP.Threads where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

-- | Makes asynchronous message handler for handling hard tasks. Messages sent
-- while handling previous message are dropped except last one. Last message is
-- always handled.
makeSequencer
  :: (a -> IO ())
  -> IO (a -> IO ())
makeSequencer handler = do
  msgVar <- newEmptyTMVarIO
  let
    sender msg = atomically $ do
      done <- tryPutTMVar msgVar msg
      unless done $ void $ swapTMVar msgVar msg
    worker = forever $ do
      msg <- atomically $ takeTMVar msgVar
      handler msg
  void $ forkIO worker
  return sender
