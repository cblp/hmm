{-# LANGUAGE DeriveGeneric #-}

module Game.Message (
        Message (..)
    ) where

import GHC.Generics
import Codec.Serialise

data Message =
      -- FIXME: add message types
      NewPlayer { np_name :: String }
  deriving (Show, Generic)

instance Serialise Message
