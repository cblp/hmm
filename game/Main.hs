{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-- import System.Random
-- import Control.Monad
-- import Graphics.Gloss.Data.Bitmap
import Apecs
import Apecs.Gloss
import Game.World
import Linear
import System.Exit

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

initialize :: System' ()
initialize = do
  _enemy1 <-
    newEntity
      (Machine
      , Position (V2 50 50)
      , Velocity 0
      , radiusDirection 1 (pi/4)
      , Skin red
      )
  _enemy2 <-
    newEntity
      ( Machine
      , Position (V2 (-50) (-50))
      , Velocity 0
      , radiusDirection 1 (pi/2)
      , Skin red)
  _player <-
    newEntity
      ( Player
      , Machine
      , Position 0
      , Velocity 0
      , AcceleratePedal False
      , BrakePedal False
      , radiusDirection 1 (pi/2)
      , Skin white)
  pure ()

worldWidth, worldHeight :: Int
worldWidth = 1000

worldHeight = 800

machineSize :: Float
machineSize = 100

draw :: System' Picture
draw = do
  machines <-
    foldDraw $ \(Machine, pos, Direction (V2 dx dy), skin) ->
      translatePos pos $
      drawMachine
        skin
        (circle (machineSize / 2) <>
         scale' (machineSize / 2) (line [(0, 0), (dx, dy)]))
  pure machines

scale' :: Float -> Picture -> Picture
scale' factor = scale factor factor

handleEvent :: Event -> System' ()
handleEvent =
  \case
    EventKey (SpecialKey KeyUp) state _ _ ->
      cmap $ \Player -> AcceleratePedal (state == Down)
    EventKey (SpecialKey KeyDown) state _ _ ->
      cmap $ \Player -> BrakePedal (state == Down)
    EventKey (SpecialKey KeyEsc) Down _ _ -> liftIO exitSuccess
    _ -> pure ()

step :: Float -> System' ()
step dT = do
  incrTime dT
  cameraFollowsThePlayer
  cmap
    $ \(Player, velocity@(Velocity v), Direction d, AcceleratePedal a, BrakePedal b) ->
      if
        | a && not b -> Velocity (v + acceleration * dT *^ d)
        | b -> decelerate dT velocity brakeFriction
        | otherwise -> Velocity v
  cmap $ \v -> decelerate dT v terrainFriction
  cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

decelerate :: Float -> Velocity -> Float -> Velocity
decelerate dT (Velocity v) friction =
  let v' = v - friction * dT *^ normalize v
   in Velocity $ if dot v v' > 0 then v' else 0

translatePos :: Position -> Picture -> Picture
translatePos (Position (V2 x y)) = translate x y

drawMachine :: Skin -> Picture -> Picture
drawMachine (Skin skin) = color skin

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

cameraFollowsThePlayer :: System' ()
cameraFollowsThePlayer =
  cmapM_ $ \(Player, Position p) ->
    modify global $ \(Camera _ s) -> Camera p s

acceleration :: Float
acceleration = 100

brakeFriction :: Float
brakeFriction = 200

terrainFriction :: Float
terrainFriction = 50
