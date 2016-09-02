module JFP.Model where

import Control.Monad
import Data.Time
import Data.Time.Clock.POSIX
import System.Environment

data JFPInput = JFPInput [FilePath]

parseArgs :: IO JFPInput
parseArgs = do
  args <- getArgs
  when (null args) $ fail "no files got"
  return $ JFPInput args

--  FIXME: parse timespec
data TimeSpec = TimeSpec String

data Follow
  = NoFollow
    -- ^ do not update
  | Follow Int
    -- ^ update every n secs

data Model = Model
  { filesToPrint :: [FilePath]
  , timeStart    :: TimeSpec
  , timeEnd      :: TimeSpec
  , follow       :: Follow
  , lastTick     :: UTCTime
  }

mkModel :: JFPInput -> Model
mkModel (JFPInput fps) = Model
  { filesToPrint = fps
  , timeStart    = TimeSpec "now-1h"
  , timeEnd      = TimeSpec "now"
  , follow       = NoFollow
  , lastTick     = posixSecondsToUTCTime 0
  }
