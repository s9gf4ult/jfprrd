module JFP.Controller where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Lens
import Control.Monad.Base
import Data.Time
import Data.UUID.V4 (nextRandom)
import Graphics.UI.Gtk
import JFP.Model
import JFP.View
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process

data Controller = Controller
  { controllerInit         :: !(IO ())
    -- ^ signaled once at start
  , controllerTick         :: !(IO ())
    -- ^ executed every sec to update window (or to not)
  , controllerResize       :: !(Allocation -> IO ())
    -- ^ window resized
  , controllerFollowToggle :: !(IO ())
    -- ^ button Follow is toggled
  }

-- | Calls redraw chart in main loop asyncronously
redrawChart :: View -> Model -> IO ()
redrawChart view model = do
  file <- do
    temp <- getTemporaryDirectory
    u <- nextRandom
    let fname = temp </> show u
    callProcess "rrdtool"
      [ "graph"
      , "-a", "PNG", "-D"
      , "-w", (show $ model ^. imageSize . _1)
      , "-h", (show $ model ^. imageSize . _2)
      , "-e", "now", "-s", "now-5h"
      , fname
      , "DEF:cpu=/var/lib/collectd/localhost/sensors-k10temp-pci-00c3/temperature-temp1.rrd:value:AVERAGE"
      , "LINE1:cpu#000000:cputemp" ]
    return fname
  postGUIAsync $ finally
    (imageSetFromFile (viewImage view) file)
    (removeFile file)

mkController :: View -> Model -> IO Controller
mkController view model' = do
  m <- newTVarIO model'
  let
    controllerInit = do
      model <- readTVarIO m
      redrawChart view model
    controllerTick = do
      now <- getCurrentTime
      work <- atomically $ do
        model <- readTVar m
        case model ^. follow of
          NoFollow -> return $ return ()
          Follow sec -> do
            let spent = diffUTCTime now $ model ^. lastTick
            if spent > sec
              then do
                writeTVar m $ model & lastTick .~ now
                return $ redrawChart view model
              else return $ return ()
      work
    controllerResize (Rectangle _ _ width height) = do
      print "resize"
      work <- atomically $ do
        model <- readTVar m
        let newm = model & imageSize .~ (width, height)
        writeTVar m newm
        return $ redrawChart view newm
      work
    controllerFollowToggle = return ()
  return Controller{..}

connectSignals :: JFPInput -> Builder -> IO View
connectSignals input bld = do
  view <- mkView bld
  contr <- mkController view $ mkModel input

  on (viewMainWindow view) deleteEvent (liftBase mainQuit *> return False)
  on (viewFollowButton view) toggled
    (controllerFollowToggle contr)
  on (viewImage view) sizeAllocate
    (controllerResize contr)
  forkIO $ do
    threadDelay 1000000             -- wait for a sec
    controllerTick contr
  controllerInit contr

  return view
