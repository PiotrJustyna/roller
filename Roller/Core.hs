module Roller.Core (main) where

import Roller.Types
import Roller.Parse
import Roller.CLI

import Control.Applicative
import Control.Monad (join, replicateM, replicateM_)
import System.Environment (getArgs)
import Data.Word

rollEm :: CLI (IO ())
rollEm verbose n args = maybe parseFail rollMany (parse input)
  where
    input             = concat args
    rollMany          = replicateM_ n . rollOnce
    rollOnce exp      = summary <$> (rolls exp) >>= putStrLn
    summary           = if verbose then showVerbose else show . sum
    showVerbose       = (\x -> (show . sum $ x) ++ " " ++ show x)
    parseFail         = putStrLn $ "Could not parse \"" ++ input ++ "\" as dice expression!"

main :: IO ()
main = join . withOpts $ rollEm
