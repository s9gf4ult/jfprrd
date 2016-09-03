module JFP.Model where

import Control.Lens
import JFP.Types
import Control.Monad
import Data.Time
import System.Environment

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

data ImageSize = ImageSize
  { _isWidth  :: Int
  , _isHeight :: Int
  }

makeLenses ''ImageSize

data Model = Model
  { _modelFiles     :: ![FilePath]
  , _modelStart     :: !TimeSpec
  , _modelEnd       :: !TimeSpec
  , _modelStep      :: !(Maybe TimeSpec)
  , _modelImageSize :: !ImageSize
  }

makeLenses ''Model

mkModel :: JFPInput -> Model
mkModel (JFPInput fps) = Model
  { _modelFiles     = fps
  , _modelStart     = TimeSpec "now-1h"
  , _modelEnd       = TimeSpec "now"
  , _modelStep      = Nothing
  , _modelImageSize = ImageSize 600 400
  }
