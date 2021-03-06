{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}

import Prelude hiding (log)
import Apecs (Entity, global, modify, cmap, cmapM_, liftIO, newEntity, runWith)
import Apecs.Gloss as G
import Linear
import System.Exit
import Colog (pattern I, withLogTextFile, log)
import Control.Concurrent (putMVar, readMVar)
import Control.Monad.Reader (ask)
import Control.Lens
import Data.Default.Class (def)
import System.Directory (createDirectoryIfMissing)

import qualified Data.HashMap.Strict as HM
import qualified SDL
import qualified SDL.Mixer as Mix

import Game (Game, MonadGame, newEnv, runGame, _assets)
import Game.World
import Game.Assets (loadSharedAssets)
import qualified Game.Assets as Assets
import qualified Game.Image as Image
import qualified Game.Level as Level
import Game.Image (Image(..))
import Game.Config (width, height, startLevel)
import Game.Env (Env, assets, config, currentLevel)
import qualified CLI

-- type Kinetic = (Position, Velocity)

main :: IO ()
main = do
  cfg <- CLI.parse
  let logsDir = "logs"
      logPath = logsDir <> "/hmm.log"
  createDirectoryIfMissing True logsDir
  withLogTextFile logPath $ \logger -> do
    SDL.initialize [SDL.InitAudio]
    Mix.withAudio def 256 $ do
      Mix.setChannels 32 -- 128 max
      Mix.whenChannelFinished $ \c ->
        putStrLn $ show c <> " finished playing!"

      env <- newEnv logger cfg
      runGame env game
    SDL.quit

game :: MonadGame m => m ()
game = do
  log I "starting game"

  cfg <- view config
  let title = "Haskell Micro Machines"
  let size = (cfg ^. width, cfg ^. height)
  let window = InWindow title size (10, 10)

  sharedAssets <- loadSharedAssets
  view assets >>= liftIO . flip putMVar sharedAssets

  lvl <- Level.load $ cfg ^. startLevel
  view currentLevel >>= liftIO . flip putMVar lvl

  carRed1 <- liftIO $ Image.load "assets/images/vehicles/car_red_1.png" PNG
  carGreen2 <- liftIO $ Image.load "assets/images/vehicles/car_green_2.png" PNG
  carYellow3 <- liftIO $ Image.load "assets/images/vehicles/car_yellow_3.png" PNG

  world <- liftIO initWorld

  env <- ask
  liftIO $ runWith world $ do
    sequence_
      [ initPlayer carRed1
      , newCar carGreen2 $ Position (V2 100 0)
      , newCar carYellow3 $ Position (V2 0 100)
      ]
    play window black 60 (draw env) (handleEvent env) step
  log I "exiting game"

initPlayer :: Picture -> System' Entity
initPlayer pic = newEntity
  ( Player
  , Machine
  , (Position 0, Velocity 0)
  , (AcceleratePedal False, BrakePedal False)
  , Direction (pi/2)
  , Skin pic
  , (SteerLeft False, SteerRight False)
  )

newCar :: Picture -> Position -> System' Entity
newCar pic pos =
  newEntity
    ( Machine
    , pos
    , Velocity 0
    , Direction (pi/4)
    , Skin pic
    )

draw :: Env Game -> System' Picture
draw _ =
  foldDraw $
    \(Machine, pos, Skin skin, Direction a, Velocity v) ->
      translatePos pos
      $ mconcat
          [ scale' 0.5 $ G.rotate (-a*180/pi + 90) skin
          , scale' 100 $ color red $ line' $ angle a
          , color blue $ line' v
          ]

line' :: V2 Float -> Picture
line' (V2 x y) = line [(0, 0), (x, y)]

scale' :: Float -> Picture -> Picture
scale' factor = scale factor factor

handleEvent :: Env Game -> Event -> System' ()
handleEvent env = \case
  EventKey (SpecialKey KeyUp) state _ _ ->
    cmap $ \Player -> AcceleratePedal (state == Down)

  EventKey (SpecialKey KeyDown) state _ _ -> do
    cmap $ \Player -> BrakePedal (state == Down)
    if state == Down then do
      assets <- liftIO . readMVar $ env ^. assets
      let sfx = Assets._sounds assets
      case HM.lookup "tires" sfx of
        Just chunk ->
          Mix.fadeIn 200 chunk
        Nothing ->
          error $ mappend "tires not found in " . show $ HM.keys sfx
    else
      Mix.fadeOut 100 Mix.AllChannels

  EventKey (SpecialKey KeyLeft) state _ _ ->
    cmap $ \Player -> SteerLeft (state == Down)

  EventKey (SpecialKey KeyRight) state _ _ ->
    cmap $ \Player -> SteerRight (state == Down)

  EventKey (SpecialKey KeyEsc) Down _ _ ->
    liftIO exitSuccess

  _ ->
    pure ()

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
