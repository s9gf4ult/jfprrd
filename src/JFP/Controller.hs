module JFP.Controller where

import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Lens
  ( (^.), (^?), (.~), (&), _Just )
import Control.Monad
import Control.Monad.Base
import Data.Maybe
import Data.UUID.V4 (nextRandom)
import Graphics.UI.Gtk as Gtk
import JFP.Cmd
import JFP.Model
import JFP.Throttle
import JFP.View
import System.Directory
import System.FilePath
import System.IO
import System.Process

import qualified Data.Text as T

data RefreshMsg = RefreshMsg
  { refreshForce     :: !Bool
  , refreshRectangle :: !Rectangle
  }

data Controller = Controller
  { controllerInit    :: !(IO ())
    -- ^ signaled once at start
  , controllerRefresh :: !(IO ())
  , controllerResize  :: !(Allocation -> IO ())
  }

-- | Calls redraw chart in main loop asyncronously
redrawChart
  :: View
  -> (TVar Model, Model)
  -- ^ To set fallback model to
  -> Model
  -- ^ New model
  -> IO ()
redrawChart view (fbVar, fbModel) model =
  try (bracket mkFile removeFile work) >>= \case
    Left (SomeException e) -> do
      hPrint stderr e
      revertView view fbModel
      atomically $ do
        oldModel <- readTVar fbVar
        when (oldModel == model) $
          writeTVar fbVar fbModel
    Right () -> return ()
  where
    mkFile = do
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
    work file = imageSetFromFile (viewImage view) file

revertView :: View -> Model -> IO ()
revertView view model = do
  Gtk.set (viewStart view) [entryText := model ^. modelStart . _TimeSpec]
  Gtk.set (viewEnd view) [entryText := model ^. modelEnd . _TimeSpec]
  Gtk.set (viewStep view)
    [entryText := (fromMaybe "" $ model ^? modelStep . _Just . _TimeSpec)]
  widgetSizeAllocate (viewImageContainer view)
    $ Rectangle 0 0
    (model ^. modelImageSize . isWidth)
    (model ^. modelImageSize . isHeight)

mkController :: View -> Model -> IO Controller
mkController view model' = do
  m <- newTVarIO model'
  revertView view model'
  let
    refresh (RefreshMsg forced (Rectangle _ _ width height)) = do
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
        return $ when (forced || (newModel /= model))
          $ redrawChart view (m, model) newModel
      work
  refreshThrottler <- makeThrottler 0.5 refresh -- every 0.5 secs
  let
    controllerInit = do
      model <- readTVarIO m
      redrawChart view (m, model') model
    controllerRefresh = do
      alloc <- widgetGetAllocation (viewImageContainer view)
      refreshThrottler $ RefreshMsg True alloc
    controllerResize alloc = refreshThrottler $ RefreshMsg False alloc
  return Controller{..}

connectSignals :: JFPInput -> Builder -> IO View
connectSignals input bld = do
  view <- mkView bld
  contr <- mkController view =<< mkModel input

  _ <- on (viewMainWindow view) deleteEvent (liftBase mainQuit *> return False)
  _ <- on (viewImageContainer view) sizeAllocate (controllerResize contr)
  _ <- on (viewRefresh view) buttonActivated (controllerRefresh contr)
  _ <- on (viewStart view) entryActivated (controllerRefresh contr)
  _ <- on (viewEnd view) entryActivated (controllerRefresh contr)
  _ <- on (viewStep view) entryActivated (controllerRefresh contr)

  controllerInit contr
  return view
