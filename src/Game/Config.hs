module Game.Config
  ( Config(..)
  ) where

import Data.Text (Text)

-- | Game configuration options.
data Config = Config
  { width :: !Int    -- ^ Window width
  , height :: !Int   -- ^ Window height
  , level :: !Text   -- ^ Initial level
  } deriving (Show)
