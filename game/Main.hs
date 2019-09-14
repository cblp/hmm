{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Game.Data.TiledMap
import Game.Data.FPS

-- import System.Random
-- import Control.Monad
-- import Graphics.Gloss.Data.Bitmap
import Apecs
import Apecs.Gloss
import Linear
import System.Exit

newtype Position =
  Position (V2 Float)
  deriving (Show)

instance Component Position where
  type Storage Position = Map Position

newtype Direction =
  Direction (V2 Float)
  deriving (Show)

instance Component Direction where
  type Storage Direction = Map Direction

newtype Velocity =
  Velocity (V2 Float)
  deriving (Show)

instance Component Velocity where
  type Storage Velocity = Map Velocity

newtype Accelerator =
  Accelerator Bool
  deriving (Show)

instance Component Accelerator where
  type Storage Accelerator = Map Accelerator

newtype Time =
  Time Float
  deriving (Show, Num)

instance Semigroup Time where
  (<>) = (+)

instance Monoid Time where
  mempty = 0

instance Component Time where
  type Storage Time = Global Time

data Player =
  Player
  deriving (Show)

instance Component Player where
  type Storage Player = Unique Player

data Machine =
  Machine
  deriving (Show)

instance Component Machine where
  type Storage Machine = Map Machine

newtype Skin =
  Skin Color
  deriving (Show)

instance Component Skin where
  type Storage Skin = Map Skin

makeWorld
  "World"
  [ ''Accelerator
  , ''Camera
  , ''Direction
  , ''Machine
  , ''Player
  , ''Position
  , ''Time
  , ''Skin
  , ''Velocity
  ]

type System' a = System World a

-- type Kinetic = (Position, Velocity)
main :: IO ()
main
  -- assets <- loadAssets
 = do
  w <- initWorld
  runWith w $ do
    initialize
    play
      (InWindow "Haskell Micro Machines" (worldWidth, worldHeight) (10, 10))
      black
      60
      draw {- assets -}
      handleEvent
      step

sqrt2 :: Float
sqrt2 = sqrt 2

initialize :: System' ()
initialize = do
  _enemy1 <-
    newEntity
      (Machine, Position (V2 50 50), Velocity 0, Direction $ V2 (1/sqrt2) (1/sqrt2), Skin red)
  _enemy2 <-
    newEntity
      ( Machine
      , Position (V2 (-50) (-50))
      , Velocity 0
      , Direction $ V2 1 0
      , Skin red)
  _player <-
    newEntity
      ( Player
      , Machine
      , Position 0
      , Velocity 0
      , Accelerator False
      , Direction $ V2 0 1
      , Skin white)
  pure ()

worldWidth, worldHeight :: Int
worldWidth = 1000

worldHeight = 800

machineSize :: Float
machineSize = 100

draw :: System' Picture
draw = do
  player <-
    foldDraw $ \(Machine, pos, Direction (V2 dx dy), skin) ->
      translatePos pos $
      drawMachine
        skin
        (circle (machineSize / 2) <>
         scale' (machineSize / 2) (line [(0, 0), (dx, dy)]))
  pure player

scale' :: Float -> Picture -> Picture
scale' factor = scale factor factor

handleEvent :: Event -> System' ()
handleEvent =
  \case
    EventKey (SpecialKey KeyUp) Down _ _ -> cmap $ \Player -> Accelerator True
    EventKey (SpecialKey KeyUp) Up _ _ -> cmap $ \Player -> Accelerator False
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
step dT = do
  incrTime dT
  cmap $ \(Velocity v, Direction d, Accelerator a) ->
    if a
      then Velocity (v + acceleration * dT *^ d)
      else Velocity v
  cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

translatePos :: Position -> Picture -> Picture
translatePos (Position (V2 x y)) = translate x y

drawMachine :: Skin -> Picture -> Picture
drawMachine (Skin skin) = color skin

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

acceleration :: Float
acceleration = 100
