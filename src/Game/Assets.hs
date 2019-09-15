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
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as Text
import Control.Lens (makeLenses)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Graphics.Gloss.Data.Picture (Picture)
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, (</>), isExtensionOf)
import Colog (WithLog, Message, pattern D, log)

import qualified Game.Audio as Audio

-- | Represents the asset source to load the `AssetMap` from
-- (can be shared or level-specific).
data AssetSource
  = Shared
  | Level Text

-- | Supported game asset types.
data AssetType
  = Sounds
  | Music
  | Pictures
  | Maps

type AssetMap a = HashMap Text a

data Assets = Assets
  { _sounds   :: AssetMap Audio.Sound
  , _music    :: AssetMap Audio.Music
  , _pictures :: AssetMap Picture
  }

makeLenses ''Assets

instance Semigroup Assets where
  a <> b = Assets
    (_sounds a <> _sounds b)
    (_music a <> _music b)
    (_pictures a <> _pictures b)

instance Monoid Assets where
  mempty = Assets mempty mempty mempty

-- | Loads game shared (global) assets.
loadSharedAssets :: (WithLog env Message m, MonadIO m) => m Assets
loadSharedAssets = do
  log D "loading shared assets..."
  Assets
    <$> load Sounds
    <*> load Music
    <*> load Pictures
  where
    load = loadAssetMap Shared

-- | Loads level assets.
loadLevelAssets :: (WithLog env Message m, MonadIO m) => Text -> m Assets
loadLevelAssets levelName = do
  log D $ "loading " <> levelName <> " level assets..."
  Assets
    <$> load Sounds
    <*> load Music
    <*> load Pictures
  where
    load = loadAssetMap $ Level levelName

loadAssetMap
  :: (WithLog env Message m, MonadIO m)
  => AssetSource
  -> AssetType
  -> m (AssetMap a)
loadAssetMap src typ = do
  let dirPath = assetsDir src typ
  log D $ "loading: " <> Text.pack dirPath
  assetPaths <- liftIO $ getDirectoryContents dirPath
  let assetFiles = filter (ext `isExtensionOf`) assetPaths
  liftIO $ print $ show assetFiles
  -- TODO: Load each asset depending on the `AssetType` given
  return HashMap.empty
  where
   ext = assetTypeExt typ

assetsDir :: AssetSource -> AssetType -> FilePath
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
