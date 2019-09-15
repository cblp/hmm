{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Assets
  ( Assets(..)
  , AssetType(..)
  , AssetSource(..)
  , AssetMap
    -- * Lenses
  , sounds
  , music
  , pictures
    -- * Operations
  , loadSharedAssets
  , loadLevelAssets
  , loadAssetMap
  , assetsDir
  , sourcePath
  , rootPath
  ) where

import Prelude hiding (log)
import Data.Traversable (for)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as Text
import Control.Lens (makeLenses)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Graphics.Gloss.Data.Picture (Picture)
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, (</>), isExtensionOf, dropExtension)
import Colog (WithLog, Message, pattern D, log)
import qualified SDL.Mixer as SDL

import qualified Game.Audio as Audio
import qualified Game.Image as Image

-- | Represents the asset source to load the `AssetMap` from
-- (can be shared or level-specific).
data AssetSource
  = Shared
  | Level Text
  deriving (Show)

-- | Supported game asset types.
data AssetType
  = Sounds
  | Music
  | Pictures
  | Maps
  deriving (Show)

type AssetMap a = HashMap Text a

data Assets = Assets
  { _sounds :: AssetMap SDL.Chunk
  , _music :: AssetMap SDL.Music
  , _pictures :: AssetMap Picture
  } deriving (Show)

makeLenses ''Assets

instance Semigroup Assets where
  a <> b = Assets
    (_sounds a <> _sounds b)
    (_music a <> _music b)
    (_pictures a <> _pictures b)

instance Monoid Assets where
  mempty = Assets mempty mempty mempty

class AssetStuff a where
  loadAsset :: FilePath -> IO a

instance AssetStuff SDL.Chunk where
  loadAsset fp =
    SDL.load fp

instance AssetStuff SDL.Music where
  loadAsset fp =
    error "TODO: loadAsset Music"

instance AssetStuff Picture where
  loadAsset fp =
    Image.load fp Image.PNG

-- | Loads game shared (global) assets.
loadSharedAssets :: (WithLog env Message m, MonadIO m) => m Assets
loadSharedAssets = do
  log D "loading shared assets..."
    <$> loadAssetMap Shared Sounds
    <*> loadAssetMap Shared Music
    <*> loadAssetMap Shared Pictures

-- | Loads level assets.
loadLevelAssets :: (WithLog env Message m, MonadIO m) => Text -> m Assets
loadLevelAssets levelName = do
  log D $ "loading " <> levelName <> " level assets..."
  Assets
    <$> loadAssetMap (Level levelName) Sounds
    <*> loadAssetMap (Level levelName) Music
    <*> loadAssetMap (Level levelName) Pictures

loadAssetMap
  :: (WithLog env Message m, MonadIO m, AssetStuff a)
  => AssetSource
assetsDir src typ = sourcePath src </> assetTypeName typ

assetTypeName :: AssetType -> String
assetTypeName = \case
  Sounds   -> "sounds"
  Music    -> "music"
  Pictures -> "images"
  Maps     -> "maps"

assetTypeExt :: AssetType -> String
assetTypeExt = \case
  Sounds   -> "wav"
  Music    -> "mp3"
  Pictures -> "png"
  Maps     -> "json"

-- | Assets source path (shared or level-specific).
sourcePath :: AssetSource -> FilePath
sourcePath Shared       = rootPath
sourcePath (Level name) = rootPath </> "levels" </> Text.unpack name

-- | Assets root directory.
rootPath :: FilePath
rootPath = "assets"
