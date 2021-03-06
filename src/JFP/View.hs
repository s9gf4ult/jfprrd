module JFP.View where

import Graphics.UI.Gtk

data View = View
  { viewMainWindow     :: !Window
  , viewImage          :: !Image
  , viewImageContainer :: !Widget
  , viewRefresh        :: !Button
  , viewStart          :: !Entry
  , viewEnd            :: !Entry
  , viewStep           :: !Entry
  }

mkView :: Builder -> IO View
mkView bld = do
  viewMainWindow <- builderGetObject bld castToWindow "main"
  viewImage <- builderGetObject bld castToImage "image"
  viewImageContainer <- builderGetObject bld castToWidget "imageContainer"
  viewRefresh <- builderGetObject bld castToButton "refresh"
  viewStart <- builderGetObject bld castToEntry "start"
  viewEnd <- builderGetObject bld castToEntry "end"
  viewStep <- builderGetObject bld castToEntry "step"
  return View {..}
