{-# LANGUAGE RecordWildCards #-}

module Game.Level where

import Data.Text (Text)
import qualified Game.Data.Assets as Assets
import Game.Data (Assets)

data Level = Level
  { name :: Text
  , assets :: Assets
  }

load :: Text -> IO Level
load name = do
  assets <- Assets.load name
  return Level{..}
