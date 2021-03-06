{-# LANGUAGE OverloadedStrings #-}

module JFP.Model where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Time
import System.Environment
import System.Process

import qualified Data.Set as S
import qualified Data.Text as T

data JFPInput = JFPInput [FilePath]

parseArgs :: IO JFPInput
parseArgs = do
  args <- getArgs
  when (null args) $ fail "no files got"
  return $ JFPInput args

data Follow
  = NoFollow
    -- ^ do not update
  | Follow NominalDiffTime
    -- ^ update every n secs

--  FIXME: parse timespec
data TimeSpec = TimeSpec String
              deriving Eq

makePrisms ''TimeSpec


data File = File
  { _fileName :: !FilePath
  , _fileDS   :: ![String]
  } deriving (Eq)

makeLenses ''File

data ImageSize = ImageSize
  { _isWidth  :: !Int
  , _isHeight :: !Int
  } deriving (Eq)

makeLenses ''ImageSize

data Model = Model
  { _modelFiles     :: ![File]
  , _modelStart     :: !TimeSpec
  , _modelEnd       :: !TimeSpec
  , _modelStep      :: !(Maybe TimeSpec)
  , _modelImageSize :: !ImageSize
  } deriving (Eq)

makeLenses ''Model

analyzeFile :: FilePath -> IO File
analyzeFile fp = do
  out <- readProcess "rrdtool"
    ["info", fp] ""
  let
    t = T.pack out
    infoLines  = map T.strip $ T.lines t
    dss = S.toList $ S.fromList $ catMaybes $ map findDs infoLines
  return $ File fp dss
  where
    findDs t = case T.splitAt 3 t of
      ("ds[", rest) -> case T.break (== ']') rest of
        (_, "") -> Nothing
        (ds, _) -> Just $ T.unpack ds
      _ -> Nothing


mkModel :: JFPInput -> IO Model
mkModel (JFPInput fps) = do
  _modelFiles <- traverse analyzeFile fps
  let
    _modelStart     = TimeSpec "now-1h"
    _modelEnd       = TimeSpec "now"
    _modelStep      = Nothing
    _modelImageSize = ImageSize 600 400
  return Model {..}
