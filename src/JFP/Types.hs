module JFP.Types where

import Control.Lens

--  FIXME: parse timespec
data TimeSpec = TimeSpec String

makePrisms ''TimeSpec
