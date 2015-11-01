module Roller.CLI (
  CLI(..)
, withOpts
) where

import Options.Applicative
import Control.Applicative

type CLI a = Bool -> Int -> [String] -> a

withOpts :: CLI a -> IO a
withOpts f = execParser . info (helper <*> handleOpts) $ infoMod
  where
  handleOpts =
    f <$> switch ( long "verbose"
                <> short 'v'
                <> help "List out each roll")
      <*> option auto ( long "nrolls"
                <> value 1
                <> short 'n'
                <> help "Number of times to roll the expression")
      <*> some (argument str (metavar "EXPR.." <> help "Dice expressions"))
  infoMod = fullDesc <> progDesc "Roll some dice"
