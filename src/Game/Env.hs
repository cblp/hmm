{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Env
  ( -- * The @Env@ type
    Env(..)
    -- * Operations
  , newEnv
    -- * Lenses
  , logger
  , assets
  , currentLevel
  , config
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Control.Lens (makeLenses, (.~))
import Control.Concurrent (MVar, newEmptyMVar)
import Colog (HasLog, LogAction, Message, cmapM, defaultFieldMap,
              fmtRichMessageDefault, getLogAction, liftLogIO, logTextStdout,
              setLogAction, upgradeMessageAction)

import Game.Assets (Assets)
import Game.Config (Config)
import Game.Level (Level)

-- | Global environment stores read-only information and
-- mutable refs to global state available to any
-- function with the `MonadReader` constraint.
data Env m = Env
  { -- | A `LogAction` to be used by the `co-log` package.
    _logger :: !(LogAction m Message)
    -- | Shared assets that are not specific to any level.
  , _assets :: !(MVar Assets)
    -- | Reference to a current level.
  , _currentLevel :: !(MVar Level)
    -- | Game configuration options.
  , _config :: !Config
  }

makeLenses ''Env

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = _logger

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction action env = env & logger .~ action

-- | Creates a record that matches the `Env` type our
-- application requires by filling in necessary fields.
newEnv
  :: MonadIO m
  => LogAction IO Text
  -> Config
  -> IO (Env m)
newEnv logTextFile cfg = Env
  <$> pure logger'
  <*> newEmptyMVar
  <*> newEmptyMVar
  <*> pure cfg
  where
    logger' = liftLogIO logFull
    logFull = upgradeMessageAction defaultFieldMap logRich
    logRich = cmapM fmtRichMessageDefault logText
    logText = logTextStdout <> logTextFile
