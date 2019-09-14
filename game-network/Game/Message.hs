module Game.Message (
        Message (..)
    ) where


newtype Message =
      -- FIXME: add message types
      NewPlayer { np_name :: String }
  deriving Show
