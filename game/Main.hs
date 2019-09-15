{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}

-- import System.Random
-- import Control.Monad
-- import Graphics.Gloss.Data.Bitmap
import Prelude hiding (log)
import Apecs (global, modify, cmap, cmapM_, liftIO, newEntity, runWith)
import Apecs.Gloss as G
import Linear
import System.Exit
import Colog (pattern I, withLogTextFile, log)
import Control.Monad.Reader (runReaderT, ask, lift)
import Control.Lens

import Game (Game, MonadGame, newEnv, runGame)
import Game.World
import qualified Game.Assets as Assets
import qualified Game.Image as Image
import qualified Game.Level as Level
import Game.Image (Image(..))
import Game.Config (width, height, startLevel)
import Game.Env (Env, config)
import qualified CLI
import System.Directory (createDirectoryIfMissing)

-- type Kinetic = (Position, Velocity)

main :: IO ()
main = do
  cfg <- CLI.parse
  let logsDir = "logs"
      logPath = logsDir <> "/hmm.log"
  createDirectoryIfMissing True logsDir
  withLogTextFile logPath $ \logger -> do
    lvl <- Level.load $ cfg ^. startLevel
    env <- newEnv logger cfg lvl
    runGame env game

game :: MonadGame m => m ()
game = do
  log I "starting game"
  x <- Assets.loadShared
  cfg <- view config
  car <- liftIO $ Image.load "assets/images/test/car.jpg" JPG
  world <- liftIO initWorld
  env <- ask
  liftIO $ runWith world $ do
    initialize car
    let title = "Haskell Micro Machines"
    let size = (cfg ^. width, cfg ^. height)
    let window = InWindow title size (10, 10)
    play window black 60 (draw env) handleEvent step
  log I "exiting game"

initialize :: Picture ->  System' ()
initialize pic = do
  _enemy1 <-
    newEntity
      ( Machine
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
      , Direction (pi/2)
      , Skin pic
      , (SteerLeft False, SteerRight False)
      )
  pure ()

draw :: Env Game -> System' Picture
draw = runReaderT $ do
  machines <- lift . foldDraw $
    \(Machine, pos, Skin skin, Direction a, Velocity v) ->
      translatePos pos
      $ mconcat
          [ scale' 0.1 $ G.rotate (-a*180/pi + 90) skin
          , scale' 100 $ color red $ line' $ angle a
          , color blue $ line' v
          ]
  pure machines

line' :: V2 Float -> Picture
line' (V2 x y) = line [(0, 0), (x, y)]

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
          | sl && not sr && v /= 0 ->
            (Direction $ d + steering, Velocity $ rotateV2 (- steering) v)
          | sr && not sl && v /= 0 ->
            (Direction $ d - steering, Velocity $ rotateV2 steering v)
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
    modify global $ \camera -> camera{camOffset = p}

acceleration :: Float
acceleration = 100

brakeFriction :: Float
brakeFriction = 200

terrainFriction :: Float
terrainFriction = 50

steerSpeed :: Float
steerSpeed = 1
