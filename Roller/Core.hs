module Roller.Core (
    main
  , roll
) where

import Roller.Types
import Roller.Parse
import Roller.CLI

import System.Environment (getArgs)
import System.Random (randomRIO)
import Control.Applicative
import Control.Monad (join, replicateM, replicateM_)
import Data.Word

rolls :: Word8 -> Word8 -> IO [Word8]
rolls n s = replicateM (fromIntegral n) . randomRIO $ (1, s)

roll :: DiceExpression -> IO [[Word8]]
roll de =
  case de of
    Sum e1 e2 -> (++) <$> roll e1 <*> roll e2
    Constant n   -> return [[n]]
    Die n s   -> return <$> n `rolls` s

rollEm :: CLI (IO ())
rollEm verbose n args = maybe parseFail rollMany (parse input)
  where
    input        = concat args
    rollMany     = replicateM_ n . rollOnce
    rollOnce exp = fmap summary (roll exp) >>= putStrLn

    summary      = if verbose then show else show . sumRolls
    sumRolls     = sum . map sum
    parseFail    = putStrLn $ "Could not parse \"" ++ input ++ "\" as dice expression!"

main :: IO ()
main = join . withOpts $ rollEm
