{-# LANGUAGE RecordWildCards #-}

module Game.Animation
  ( Animation(..)
  , picture
  ) where

import Graphics.Gloss (Picture)

data Animation = Animation
  { pics :: [Picture]
  , start :: Float
  , delay :: Float
  }

picture :: Animation -> Float -> Maybe Picture
picture Animation{..} time
  | start > time      = Nothing
  | ix >= length pics = Nothing
  | otherwise         = Just $ pics !! ix
  where
    ix = round (dt / delay)
    dt = time - start
