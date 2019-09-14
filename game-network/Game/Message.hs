module Game.Message (
        Message (..)
    ) where


data Message =
      -- FIXME: add message types
      NewPlayer { np_name :: String }
  deriving Show
