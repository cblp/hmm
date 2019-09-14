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
import Game.Image

-- type Kinetic = (Position, Velocity)
main :: IO ()
main
  -- assets <- loadAssets
 = do
  car <- load "assets/car.jpg" JPG
  w <- initWorld
  runWith w $ do
    initialize car
    play
      (InWindow "Haskell Micro Machines" (worldWidth, worldHeight) (10, 10))
      black
      60
      draw
      handleEvent
      step

initialize :: Picture ->  System' ()
initialize pic = do
  _enemy1 <-
    newEntity
      (Machine
      , Position (V2 50 50)
      , Velocity 0
      , Direction (pi/4)
      , Skin pic
      )
  _enemy2 <-
    newEntity
      ( Machine
      , Position (V2 (-50) (-50))
      , Velocity 0
      , Direction (pi/2)
      , Skin pic)
  _player <-
    newEntity
      ( Player
      , Machine
      , (Position 0, Velocity 0)
      , (AcceleratePedal False, BrakePedal False)
      , Direction 0
      , Skin pic
      , (SteerLeft False, SteerRight False)
      )
  pure ()

worldWidth, worldHeight :: Int
worldWidth = 1000

worldHeight = 800

draw :: System' Picture
draw = do
  player <-
    foldDraw $ \(Machine, pos, Skin skin) ->
      translatePos pos $ scale' 0.1 skin
  pure player

scale' :: Float -> Picture -> Picture
scale' factor = scale factor factor

handleEvent :: Event -> System' ()
handleEvent =
  \case
    EventKey (SpecialKey KeyUp) state _ _ ->
      cmap $ \Player -> AcceleratePedal (state == Down)
    EventKey (SpecialKey KeyDown) state _ _ ->
      cmap $ \Player -> BrakePedal (state == Down)
    EventKey (SpecialKey KeyLeft) state _ _ ->
      cmap $ \Player -> SteerLeft (state == Down)
    EventKey (SpecialKey KeyRight) state _ _ ->
      cmap $ \Player -> SteerRight (state == Down)
    EventKey (SpecialKey KeyEsc) Down _ _ -> liftIO exitSuccess
    _ -> pure ()

step :: Float -> System' ()
step dT = do
  incrTime dT
  cameraFollowsThePlayer
  cmap $ \(Player, SteerLeft sl, SteerRight sr, Direction d, Velocity v) ->
    let steering = dT * steerSpeed
     in if
          | sl && not sr ->
            (Direction $ d - steering, Velocity $ rotateV2 (- steering) v)
          | sr && not sl ->
            (Direction $ d + steering, Velocity $ rotateV2 steering v)
          | otherwise -> (Direction d, Velocity v)
  cmap
    $ \(Player, velocity@(Velocity v), Direction d, AcceleratePedal a, BrakePedal b) ->
      if
        | a && not b -> Velocity (v + acceleration * dT *^ angle d)
        | b -> decelerate dT velocity brakeFriction
        | otherwise -> Velocity v
  cmap $ \v -> decelerate dT v terrainFriction
  cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

rotateV2 :: Floating a => a -> V2 a -> V2 a
rotateV2 a (V2 x y) = V2 (x * cos a + y * sin a) (- x * sin a + y * cos a)

decelerate :: Float -> Velocity -> Float -> Velocity
decelerate dT (Velocity v) friction =
  let v' = v - friction * dT *^ normalize v
   in Velocity $ if dot v v' > 0 then v' else 0

translatePos :: Position -> Picture -> Picture
translatePos (Position (V2 x y)) = translate x y

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

steerSpeed :: Float
steerSpeed = 1
