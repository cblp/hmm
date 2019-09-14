{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- import System.Random
-- import Control.Monad
-- import Graphics.Gloss.Data.Bitmap
import Apecs
import Apecs.Gloss
import Linear
import System.Exit

newtype Position = Position (V2 Float) deriving (Show)

instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving (Show)

instance Component Velocity where type Storage Velocity = Map Velocity

newtype Time = Time Float deriving (Show, Num)

instance Semigroup Time where (<>) = (+)

instance Monoid Time where mempty = 0

instance Component Time where type Storage Time = Global Time

data Player = Player deriving (Show)

instance Component Player where type Storage Player = Unique Player

data Machine = Machine deriving (Show)

instance Component Machine where type Storage Machine = Map Machine

makeWorld
  "World"
  [ ''Position,
    ''Velocity,
    ''Time,
    ''Player,
    ''Camera,
    ''Machine
    ]

type System' a = System World a

-- type Kinetic = (Position, Velocity)
main :: IO ()
main = do
  -- assets <- loadAssets
  w <- initWorld
  runWith w $ do
    initialize
    play
      (InWindow "Shmup" (worldWidth, worldHeight) (10, 10))
      black
      60
      draw {- assets -}
      handleEvent
      step

initialize :: System' ()
initialize = do
  _player <- newEntity (Player, Machine, Position 0, Velocity 0)
  pure ()

worldWidth, worldHeight :: Int
worldWidth = 600
worldHeight = 800

draw :: System' Picture
draw = pure mempty

handleEvent :: Event -> System' ()
handleEvent = \case
  -- EventKey (SpecialKey KeyLeft) Down _ _ ->
  --   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x - playerSpeed) 0)
  -- EventKey (SpecialKey KeyLeft) Up _ _ ->
  --   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x + playerSpeed) 0)
  -- EventKey (SpecialKey KeyRight) Down _ _ ->
  --   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x + playerSpeed) 0)
  -- EventKey (SpecialKey KeyRight) Up _ _ ->
  --   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x - playerSpeed) 0)
  -- EventKey (SpecialKey KeySpace) Down _ _ ->
  --   cmapM_ $ \(Player, pos :: Position) -> do
  --     _bullet <- newEntity (Bullet, pos, Velocity (V2 0 bulletSpeed))
  --     pure ()
  EventKey (SpecialKey KeyEsc) Down _ _ -> liftIO exitSuccess
  _ -> pure ()

step :: Float -> System' ()
step _dT = do
  pure ()
