module Main where

import Graphics.UI.Gtk
import JFP
import Paths_jfprrd

main :: IO ()
main = do
  _ <- initGUI
  bld <- builderNew
  xmlFile <- getDataFileName "glade/main.glade"
  builderAddFromFile bld xmlFile
  input <- parseArgs
  view <- connectSignals input bld
  widgetShowAll $ viewMainWindow view
  mainGUI
