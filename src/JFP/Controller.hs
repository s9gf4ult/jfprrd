module JFP.Controller where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.Base
import Graphics.UI.Gtk
import JFP.Model
import JFP.View

data Controller = Controller
  { controllerInit :: IO ()
    -- ^ signaled once at start
  , controllerTick         :: IO ()
    -- ^ executed every sec to update window (or to not)
  , controllerResize       :: IO ()
    -- ^ window resized
  , controllerFollowToggle :: IO ()
    -- ^ button Follow is toggled
  }

mkController :: View -> Model -> IO Controller
mkController view model = do
  m <- newTVarIO model
  error "FIXME: not implemented"



connectSignals :: JFPInput -> Builder -> IO View
connectSignals input bld = do
  view <- mkView bld
  contr <- mkController view $ mkModel input

  on (mainWindow view) deleteEvent (liftBase mainQuit *> return False)
  on (followButton view) buttonPressEvent
    (liftBase $ controllerFollowToggle contr *> return False)
  forkIO $ do
    threadDelay 1000000             -- wait for a sec
    controllerTick contr

  return view
