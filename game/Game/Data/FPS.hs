module Game.Data.FPS
  ( FPS(..)
  ) where

import Data.Word (Word32)

data FPS = FPS
  { frameCount :: Int
  , lastTime :: Word32
  , number :: Float
  } deriving (Show)
