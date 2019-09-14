module Game.Data.Assets
  ( Assets(..)
  , load
  ) where

import Data.Text (Text)
import Game.Data.TiledMap (TiledMap)

data Assets = Assets
  { sfx :: ()
  , music :: ()
  , tiledMap :: TiledMap
  }

load :: Text -> IO Assets
load levelName = undefined
