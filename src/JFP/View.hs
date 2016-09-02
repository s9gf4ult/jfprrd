module JFP.View where

import Graphics.UI.Gtk

data View = View
  { mainWindow   :: Window
  , followButton :: ToggleButton
  , followTime   :: SpinButton
  }

mkView :: Builder -> IO View
mkView bld = do
  mainWindow <- builderGetObject bld castToWindow "main"
  followButton <- builderGetObject bld castToToggleButton "follow"
  followTime <- builderGetObject bld castToSpinButton "follow_time"
  return View {..}
