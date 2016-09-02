module Main where

import Graphics.UI.Gtk
import Paths_jfprrd

main :: IO ()
main = do
  initGUI
  bld <- builderNew
  xmlFile <- getDataFileName "glade/main.glade"
  builderAddFromFile bld xmlFile
  win <- builderGetObject bld castToWindow "main"
  widgetShowAll win
  mainGUI
