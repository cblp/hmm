{-# LANGUAGE RecordWildCards #-}

module CLI
  ( parser
  , usage
  ) where

import Options.Applicative
import Game.Config (Config(..))

-- | CLI options parser.
parser :: Config -> Parser Config
parser Config{..} = Config
  <$> option auto
      ( long "width"
     <> short 'w'
     <> metavar "WIDTH"
     <> value width
     <> showDefault
     <> help "Window width"
      )
  <*> option auto
      ( long "height"
     <> short 'h'
     <> metavar "HEIGHT"
     <> value height
     <> showDefault
     <> help "Window height"
      )
  <*> strOption
      ( long "level"
     <> short 'l'
     <> metavar "LEVEL"
     <> value level
     <> showDefault
     <> help "Level to start from"
      )

-- | Describes what the program does.
-- To be displayed in the help screen.
usage :: Config -> ParserInfo Config
usage defaults = info (parser defaults <**> helper)
  (  fullDesc
  <> progDesc "Remake of the Micro Machines game in Haskell"
  <> header "hmm - haskell micro machines"
  )
