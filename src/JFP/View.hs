module JFP.View where

import Graphics.UI.Gtk

data View = View
  { viewMainWindow   :: Window
  , viewImage        :: Image
  , viewFollowButton :: ToggleButton
  , viewFollowTime   :: SpinButton
  }

mkView :: Builder -> IO View
mkView bld = do
  viewMainWindow <- builderGetObject bld castToWindow "main"
  viewImage <- builderGetObject bld castToImage "image"
  viewFollowButton <- builderGetObject bld castToToggleButton "follow"
  viewFollowTime <- builderGetObject bld castToSpinButton "follow_time"
  return View {..}
