{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CLI
  ( parser
  , usage
  , parse
  ) where

import Options.Applicative
import Game.Config (Config(..))

-- | Parses CLI options.
parse :: IO Config
parse = execParser usage

-- | CLI options parser.
parser :: Config -> Parser Config
parser Config{..} = Config
  <$> option auto
      ( long "width"
     <> short 'w'
     <> metavar "WIDTH"
     <> value _width
     <> showDefault
     <> help "Window width"
      )
  <*> option auto
      ( long "height"
     <> short 'h'
     <> metavar "HEIGHT"
     <> value _height
     <> showDefault
     <> help "Window height"
      )
  <*> strOption
      ( long "level"
     <> short 'l'
     <> metavar "LEVEL"
     <> value _startLevel
     <> showDefault
     <> help "Level to start"
      )

-- | Describes what the program does.
-- To be displayed in the help screen.
usage :: ParserInfo Config
usage = info (parser defaults <**> helper)
  (  fullDesc
  <> progDesc "Remake of the Micro Machines game in Haskell"
  <> header "hmm - haskell micro machines"
  )
  where
    defaults = Config
      { _width = 1000
      , _height = 800
      , _startLevel = "test"
      }
