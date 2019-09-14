{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Game
  ( module Game.World
  , module Game.Image
  , module Game.FPS
  , module Game.Animation
  , module Game.Data
  , module Game.Config
  , module Game.Env
  , Game(..)
  , runGame
  , MonadGame
  ) where

import Colog (Message, WithLog)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Natural (type (~>))

import Game.World
import Game.Config (Config)
import Game.Image (Image, ImageInfo)
import Game.FPS (FPS)
import Game.Animation (Animation)
import Game.Data (TiledMap)
import Game.Env (Env(..), newEnv)

newtype Game a = Game
  { unGame :: ReaderT (Env Game) IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Env Game)
    , MonadThrow
    )

runGame :: Env Game -> Game ~> IO
runGame env = flip runReaderT env . unGame

-- | Synonym for constraints commonly
-- satisfied by monads used in stack.
type MonadGame m =
  ( WithLog (Env Game) Message m
  , MonadIO m
  , MonadThrow m
  )
