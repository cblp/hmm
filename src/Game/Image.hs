{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Image
  ( Image(..)
  , ImageInfo(..)
  , load
  , loadInfo
  ) where

import Data.Maybe (fromMaybe)
import Control.Exception (Exception, throw)
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Juicy (loadJuicyJPG, loadJuicyPNG)

-- | Supported image types.
data Image
  = BMP
  | PNG
  | JPG
  deriving (Eq, Show)

data ImageInfo = ImageInfo
  { imagePath :: FilePath
  , imageType :: Image
  }

newtype ImageLoadException = ImageLoadException ImageInfo
instance Exception ImageLoadException

instance Show ImageLoadException where
  show (ImageLoadException ImageInfo{..}) = unlines
    [ "Failed to load image."
    , "Type: " <> show imageType
    , "Path: " <> imagePath
    ]

-- | Loads image using given file path and type.
load :: FilePath -> Image -> IO Picture
load imagePath imageType = loadInfo ImageInfo{..}

loadInfo :: ImageInfo -> IO Picture
loadInfo info@ImageInfo{..} = case imageType of
  BMP -> loadBMP imagePath
  PNG -> loadWith loadJuicyPNG
  JPG -> loadWith loadJuicyJPG
  where
    loadWith :: (FilePath -> IO (Maybe Picture)) -> IO Picture
    loadWith loader = do
      pic <- loader imagePath
      return $ fromMaybe (throw $ ImageLoadException info) pic
