module JFP.Controller where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Lens
import Control.Monad.Base
import Data.Maybe
import Data.Time
import Data.UUID.V4 (nextRandom)
import Graphics.UI.Gtk as Gtk
import JFP.Model
import JFP.View
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process

data Controller = Controller
  { controllerInit    :: !(IO ())
    -- ^ signaled once at start
  , controllerRefresh :: !(IO ())
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
      , "-w", (show $ model ^. modelImageSize . isWidth)
      , "-h", (show $ model ^. modelImageSize . isHeight)
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
  Gtk.set (viewStart view) [entryText := model' ^. modelStart . _TimeSpec]
  Gtk.set (viewEnd view) [entryText := model' ^. modelEnd . _TimeSpec]
  Gtk.set (viewStep view)
    [entryText := (fromMaybe "" $ model' ^? modelStep . _Just . _TimeSpec)]
  let
    controllerInit = do
      model <- readTVarIO m
      redrawChart view model
    controllerRefresh = return ()
  return Controller{..}

connectSignals :: JFPInput -> Builder -> IO View
connectSignals input bld = do
  view <- mkView bld
  contr <- mkController view $ mkModel input

  on (viewMainWindow view) deleteEvent (liftBase mainQuit *> return False)
  on (viewRefresh view) buttonActivated (controllerRefresh contr)
  on (viewStart view) entryActivated (controllerRefresh contr)
  on (viewEnd view) entryActivated (controllerRefresh contr)
  on (viewStep view) entryActivated (controllerRefresh contr)

  controllerInit contr
  return view
