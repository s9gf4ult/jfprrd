{-# LANGUAGE OverloadedStrings #-}

module JFP.Cmd where

import Control.Applicative
import Control.Lens
import Data.Bits
import Data.Foldable
import Data.Set(Set)
import Data.Word
import Formatting
import JFP.Model

import qualified Data.Set as S
import qualified Data.Text as T

data Color = Color
  { _red   :: !Word8
  , _green :: !Word8
  , _blue  :: !Word8
  } deriving (Eq, Ord)

makeLenses ''Color

renderColor :: Color -> String
renderColor c =
  T.unpack $ sformat ("#" % hex2 % hex2 % hex2)
  (c ^. red)
  (c ^. green)
  (c ^. blue)
  where
    hex2 = left 2 '0' %. hex

-- | infinite lis of colors
colors :: [Color]
colors = uniqs S.empty $ filter (/= Color 255 255 255) $ do
  multiplier <- [Color 255 255 255, Color 60 125 255, Color 255 125 60]
  --  FIXME: make it infinite
  n <- [0,1,2,4,3,6,5,7]
  return $ toColor n multiplier
  where
    toColor :: Int -> Color -> Color
    toColor n color =
      let
        r = if testBit n 0 then 1 else 0
        g = if testBit n 1 then 1 else 0
        b = if testBit n 2 then 1 else 0
      in color & (red *~ r) . (green *~ g) . (blue *~ b)
    uniqs :: (Ord a) => Set a -> [a] -> [a]
    uniqs s = \case
      [] -> []
      (a:as)
        | S.member a s -> uniqs s as
        | otherwise -> a:uniqs (S.insert a s) as

data Cmd = Cmd
  { cmdFile          :: FilePath
  , cmdName          :: String
  , cmdColor         :: Color
  , cmdLegend        :: String
  , cmdDataSource    :: String
  , cmdConsolidation :: String
  , cmdStep          :: Maybe TimeSpec
  }

-- DEF:<vname>=<rrdfile>:<ds-name>:<CF>[:step=<step>][:start=<time>][:end=<time>][:reduce=<CF>][:daemon=<address>]
cmdDef :: Cmd -> String
cmdDef Cmd{..} = T.unpack
  $ sformat ("DEF:" % string % "=" % string % ":" % string % ":" % string % string)
  cmdName cmdFile cmdDataSource cmdConsolidation rest
  where
    rest = case cmdStep of
      Nothing   -> ""
      Just step -> ":step=" ++ step ^. _TimeSpec

-- LINE[width]:value[#color][:[legend][:STACK][:skipscale][:dashes[=on_s[,off_s[,on_s,off_s]...]][:dash-offset=offset]]]
cmdLine :: Cmd -> String
cmdLine Cmd{..} = T.unpack
  $ sformat ("LINE1:" % string % string % ":" % string)
    cmdName (renderColor cmdColor) cmdLegend

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix (a:as) (b:bs)
  | a == b = a:commonPrefix as bs
  | otherwise = []
commonPrefix _ _ = []

cutPrefixes :: (Eq a) => [[a]] -> [[a]]
cutPrefixes ls = map (drop $ length prefix) ls
  where
    prefix = case ls of
      (a:b:as) -> foldl' commonPrefix a (b:as)
      (_:[])   -> []
      []       -> []

cutSuffixes :: (Eq a) => [[a]] -> [[a]]
cutSuffixes a = map reverse $ cutPrefixes $ map reverse a

-- | Generates drawing cmd params for rrdtool graph
rrdCmd :: Maybe TimeSpec -> [File] -> [String]
rrdCmd step files = map cmdDef cmds ++ map cmdLine cmds
  where
    fileCuts = cutSuffixes $ cutPrefixes $ map _fileName files
    fps = do
      (File file dss, cut) <- zip files fileCuts
      ds <- dss
      let legend = cut ++ "[" ++ ds ++ "]"
      return (file, ds, legend)
    toCmd (file, ds, legend) name color
      = Cmd file name color legend ds
    cmds = getZipList $ toCmd
      <$> ZipList fps
      <*> ZipList names
      <*> ZipList colors
      <*> ZipList (repeat "AVERAGE") --  FIXME: optional?
      <*> ZipList (repeat step)
    names = ("n" ++) . show <$> [(1::Int)..]
