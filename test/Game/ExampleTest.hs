{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Game.ExampleTest where

import Prelude hiding (Product)

import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Text as Text

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
