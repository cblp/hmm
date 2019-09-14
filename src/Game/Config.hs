{-# LANGUAGE TemplateHaskell #-}

module Game.Config
  ( Config(..)
  , width
  , height
  , startLevel
  ) where

import Control.Lens (makeLenses)
import Data.Text (Text)

-- | Game configuration options.
data Config = Config
  { _width :: !Int -- ^ Window width
  , _height :: !Int -- ^ Window height
  , _startLevel :: !Text -- ^ Initial level
  } deriving (Show)

makeLenses ''Config
