module JFP.Model where

import Control.Lens
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
  | Follow NominalDiffTime
    -- ^ update every n secs

data Model = Model
  { _filesToPrint :: [FilePath]
  , _timeStart    :: TimeSpec
  , _timeEnd      :: TimeSpec
  , _imageSize    :: (Int, Int)
  , _follow       :: Follow
  , _lastTick     :: UTCTime
  }

makeLenses ''Model

mkModel :: JFPInput -> Model
mkModel (JFPInput fps) = Model
  { _filesToPrint = fps
  , _timeStart    = TimeSpec "now-1h"
  , _timeEnd      = TimeSpec "now"
  , _imageSize    = (600, 400)
  , _follow       = NoFollow
  , _lastTick     = posixSecondsToUTCTime 0
  }
