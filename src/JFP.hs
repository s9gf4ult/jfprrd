module JFP where

import Control.Monad
import Control.Monad.Base
import Data.Time
import Data.Time.Clock.POSIX
import Graphics.UI.Gtk
import System.Environment

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

data JFPInput = JFPInput [FilePath]

data Controller = Controller
  { tick         :: IO ()
    -- ^ executed every sec to update window (or to not)
  , resize       :: IO ()
    -- ^ window resized
  , followToggle :: IO ()
    -- ^ button Follow is toggled
  }

mkModel :: JFPInput -> Model
mkModel (JFPInput fps) = Model
  { filesToPrint = fps
  , timeStart    = TimeSpec "now-1h"
  , timeEnd      = TimeSpec "now"
  , follow       = NoFollow
  , lastTick     = posixSecondsToUTCTime 0
  }

mkController :: Model
mkController model = do
  new

parseArgs :: IO JFPInput
parseArgs = do
  args <- getArgs
  when (null args) $ fail "no files got"
  return $ JFPInput args

connectSignals :: JFPInput -> Builder -> IO Window
connectSignals input bld = do
  win <- builderGetObject bld castToWindow "main"
  _ <- on win deleteEvent (liftBase mainQuit *> pure False)



  return win
