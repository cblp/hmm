{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Level
  ( Level(..)
  , name
  , assets
  , tiledMap
  , load
  ) where

import Prelude hiding (log)
import Data.Text (Text)
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO)
import System.FilePath ((</>), (<.>))
import Colog (WithLog, Message)

import Game.Image (Image(..), ImageInfo(..))
import Game.TiledMap (TiledMap(..), TiledMapInfo(..))
import qualified Game.TiledMap as TiledMap
import qualified Game.Assets as Assets
import Game.Assets (Assets(..), assetsDir, loadLevelAssets)

data Level = Level
  { _name :: Text
  , _assets :: Assets
  , _tiledMap :: TiledMap
  }

makeLenses ''Level

load :: (WithLog env Message m, MonadIO m) => Text -> m Level
load n = Level
  <$> pure n
  <*> loadLevelAssets n
  <*> loadTiledMap n

loadTiledMap :: MonadIO m => Text -> m TiledMap
loadTiledMap levelName = do
  let
    imagePath = mapsDir </> "tileset" <.> "png"
    tilesetImage = ImageInfo { imagePath, imageType = PNG }
    jsonPath = mapsDir </> "map" <.> "json"
  TiledMap.load $ TiledMapInfo { jsonPath, tilesetImage }
  where
    mapsDir = assetsDir (Assets.Level levelName) Assets.Maps
