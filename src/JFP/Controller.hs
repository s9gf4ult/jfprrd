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
import JFP.Cmd
import JFP.Model
import JFP.Types
import JFP.View
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process

import qualified Data.Text as T

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
      $ [ "graph"
        , "-a", "PNG", "-D"
        , "-w", (show $ model ^. modelImageSize . isWidth)
        , "-h", (show $ model ^. modelImageSize . isHeight)
        , "-e", (model ^. modelEnd . _TimeSpec)
        , "-s", (model ^. modelStart . _TimeSpec)
        , fname
        ]
      ++ rrdCmd (model ^. modelStep) (model ^. modelFiles)
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
    controllerRefresh = do
      Rectangle _ _ width height <- widgetGetAllocation $ viewImageContainer view
      start <- Gtk.get (viewStart view) entryText
      end <- Gtk.get (viewEnd view) entryText
      step <- Gtk.get (viewStep view) entryText
      work <- atomically $ do
        model <- readTVar m
        let
          mstep = case T.unpack $ T.strip $ T.pack step of
            "" -> Nothing
            x  -> Just $ TimeSpec x
          newModel = model
            & (modelStart .~ TimeSpec start)
            . (modelEnd .~ TimeSpec end)
            . (modelStep .~ mstep)
            . (modelImageSize .~ ImageSize width height)
        writeTVar m newModel
        return $ redrawChart view newModel
      work
  return Controller{..}

connectSignals :: JFPInput -> Builder -> IO View
connectSignals input bld = do
  view <- mkView bld
  contr <- mkController view =<< mkModel input

  on (viewMainWindow view) deleteEvent (liftBase mainQuit *> return False)
  on (viewRefresh view) buttonActivated (controllerRefresh contr)
  on (viewStart view) entryActivated (controllerRefresh contr)
  on (viewEnd view) entryActivated (controllerRefresh contr)
  on (viewStep view) entryActivated (controllerRefresh contr)

  controllerInit contr
  return view
