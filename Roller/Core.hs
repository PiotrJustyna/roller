module Roller.Core (
  module Roller.Parse
, module Roller.Types
, main
) where

import Roller.Types
import Roller.Parse

import System.Environment (getArgs)
import System.Random (randomRIO)
import Control.Applicative hiding (Const)
import Control.Monad

rolls :: Int -> Int -> IO [Int]
rolls n s = replicateM n . randomRIO $ (1,s)

roll :: DiceExp -> IO [[Int]]
roll de =
  case de of
    Sum e1 e2 -> (++) <$> roll e1 <*> roll e2
    Const n   -> return [[n]]
    Die n s   -> return <$> n `rolls` s

rollEm :: DiceExp -> Bool -> Int -> IO ()
rollEm exp verbose n = replicateM_ n $ rollOnce
  where
    summary  = if verbose then show else show . sumRolls
    rollOnce = fmap summary (roll exp) >>= putStrLn
    sumRolls = sum . map sum

main :: IO ()
main = do
  args <- fmap concat getArgs
  let parseFail  = putStrLn $ "Could not parse \"" ++ args ++ "\" as dice expression!"
      doRoll exp = rollEm exp False 1
  maybe parseFail doRoll (parse args)

