{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Image
  ( Image(..)
  , load
  ) where

import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Juicy (loadJuicyJPG, loadJuicyPNG)

-- | Supported image types.
data Image =
  | BMP
  | PNG
  | JPG
  deriving (Eq, Show)

data ImageLoadException = ImageLoadException (FilePath, Image) deriving (Eq)
instance Exception ImageLoadException

instance Show ImageLoadException where
  show (ImageLoadException (path, img)) = unlines
    [ "Failed to load image of type: "
    , show img
    , "reason: "
    , path
    ]

-- | Loads image of type given file path and type.
load :: FilePath -> Image -> IO Picture
load path msg = case msg of
  BMP -> loadBMP path
  PNG -> loadWith loadJuicyPNG
  JPG -> loadWith loadJuicyJPG
  where
    loadWith f = fromMaybe (throw $ ImageLoadException (path, msg)) (f path)
