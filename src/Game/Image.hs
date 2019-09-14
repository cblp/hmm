{-# LANGUAGE OverloadedStrings #-}

module Game.Image
  ( Image(..)
  , load
  ) where

import Data.Maybe (fromMaybe)
import Data.Maybe (fromJust)
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

newtype ImageLoadException = ImageLoadException (FilePath, Image) deriving (Eq)
instance Exception ImageLoadException

instance Show ImageLoadException where
  show (ImageLoadException (msg, img)) = unlines
    [ "Failed to load image of type: " <> show img
    , "Reason: " <> msg
    ]


-- | Loads image of type given file path and type.
load :: FilePath -> Image -> IO Picture
load path msg = case msg of
  BMP -> undefined -- loadBMP path
  PNG -> undefined -- loadWith loadJuicyPNG
  JPG -> fromJust <$> loadJuicyJPG path
  where
    loadWith :: (FilePath -> Maybe a) -> a
    loadWith f = fromMaybe (throw $ ImageLoadException (path, msg)) (f path)
