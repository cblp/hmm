{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Assets
  ( Assets(..)
  , AssetType(..)
  , AssetSource(..)
  , AssetMap
    -- * Lenses
  , sfxs
  , soundtracks
  , pictures
    -- * Operations
  , loadShared
  , load
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens (makeLenses)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Graphics.Gloss.Data.Picture (Picture)
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, (</>), (<.>))

import Game.Sound (Sfx, Soundtrack)
import Game.Animation (Animation)

-- | Supported game asset types.
data AssetType
  = Sfxs
  | Soundtracks
  | Images

assetTypeDir :: AssetType -> FilePath
assetTypeDir = \case
  Sfxs -> "sfxs"
  Soundtracks -> "soundtracks"
  Images -> "images"

-- | Represents the asset source to load the `AssetMap` from
-- (can be shared or level-specific).
data AssetSource
  = Shared
  | Level Text
type AssetMap = HashMap Text

data Assets = Assets
  { _sfxs :: AssetMap Sfx
  , _soundtracks :: AssetMap Soundtrack
  , _pictures :: AssetMap Picture
  }

makeLenses ''Assets

-- | Loads game shared (global) assets.
loadShared :: IO Assets
loadShared = do
  _sfxs <- load Shared Sfxs
  _soundtracks <- load Shared Soundtracks
  _pictures <- load Shared Images
  return Assets{..}

load :: AssetSource -> AssetType -> IO (AssetMap a)
load src typ = do
  let filePath = mkPath src typ
  dirPaths <- getDirectoryContents filePath
  -- TODO: Load each asset depending on the `AssetType` given
  return HashMap.empty

mkPath :: AssetSource -> AssetType -> FilePath
mkPath src typ = sourcePath src </> assetTypeDir typ

-- | Assets root directory.
rootPath :: FilePath
rootPath = "assets"

-- | Assets source path (shared or level-specific).
sourcePath :: AssetSource -> FilePath
sourcePath Shared = rootPath
sourcePath (Level name) = rootPath </> "levels" </> Text.unpack name
