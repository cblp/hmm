module Game.Audio
  ( Sound
  , Music
  ) where

import qualified SDL.Mixer as SDL

type Sound = SDL.Chunk

type Music = SDL.Music
