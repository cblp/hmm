{-# LANGUAGE TypeFamilies #-}

import Apecs
-- import Apecs.Gloss
-- import Control.Monad
-- import Graphics.Gloss.Data.Bitmap
import Linear
-- import System.Exit
-- import System.Random

newtype Position = Position (V2 Float) deriving (Show)

instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving (Show)

instance Component Velocity where type Storage Velocity = Map Velocity

main :: IO ()
main = pure ()
