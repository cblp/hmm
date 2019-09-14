{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Game.Env
  ( Env(..)
  , newEnv
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, newIORef)
import Colog (HasLog, LogAction, Message, cmapM, defaultFieldMap,
              fmtRichMessageDefault, getLogAction, liftLogIO, logTextStdout,
              setLogAction, upgradeMessageAction)

import Game.Config (Config)
import Game.Level (Level)

-- | Global environment stores read-only information and
-- mutable refs to global state available to any
-- function with the `MonadReader` constraint.
data Env m = Env
  { -- | A `LogAction` to be used by the `co-log` package.
    logger :: !(LogAction m Message)
    -- | Reference to a current level.
  , level :: !(IORef Level)
    -- | Game configuration options.
  , config :: !Config
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = logger

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction action env = env { logger = action }

-- | Creates a record that matches the `Env` type our
-- application requires by filling in necessary fields.
newEnv
  :: MonadIO m
  => LogAction IO Text
  -> Config
  -> Level
  -> IO (Env m)
newEnv logTextFile config lvl = do
  let
    logText = logTextStdout <> logTextFile
    logRich = cmapM fmtRichMessageDefault logText
    logFull = upgradeMessageAction defaultFieldMap logRich
    logger  = liftLogIO logFull
  level <- newIORef lvl
  return Env{..}
