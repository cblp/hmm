{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DerivingVia       #-}

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
  , loadShared
  , loadSource
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
  | Level String

-- | Supported game asset types.
data AssetType
  = Sounds
  | Music
  | Pictures

type AssetMap a = HashMap Text a

data Assets = Assets
  { _sounds :: AssetMap Audio.Sound
  , _music :: AssetMap Audio.Music
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
loadShared :: (WithLog env Message m, MonadIO m) => m Assets
loadShared = do
  log D "loading assets"
  Assets
    <$> load Sounds
    <*> load Music
    <*> load Pictures
  where
    load = loadSource Shared

loadSource
  :: (WithLog env Message m, MonadIO m)
  => AssetSource
  -> AssetType
  -> m (AssetMap a)
loadSource src typ = do
  let path = mkPath src typ
  log D $ "loading " <> Text.pack (assetSourceName src) <> " : " <> Text.pack path
  assetPaths <- liftIO $ getDirectoryContents path
  let assetFiles = filter (ext `isExtensionOf`) assetPaths
  liftIO $ print $ show assetFiles
  -- TODO: Load each asset depending on the `AssetType` given
  return HashMap.empty
  where
   ext = assetTypeExt typ

mkPath :: AssetSource -> AssetType -> FilePath
mkPath src typ = sourcePath src </> assetTypeName typ

assetSourceName :: AssetSource -> String
assetSourceName = \case
  Shared -> "shared"
  Level name -> "level " <> name

assetTypeName :: AssetType -> String
assetTypeName = \case
  Sounds -> "sounds"
  Music -> "music"
  Pictures -> "images"

assetTypeExt :: AssetType -> String
assetTypeExt = \case
  Sounds -> ".wav"
  Music -> ".mp3"
  Pictures -> ".png"


-- | Assets source path (shared or level-specific).
sourcePath :: AssetSource -> FilePath
sourcePath Shared       = rootPath
sourcePath (Level name) = rootPath </> "levels" </> name

-- | Assets root directory.
rootPath :: FilePath
rootPath = "assets"
