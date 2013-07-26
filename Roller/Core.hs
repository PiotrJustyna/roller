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

rolls :: Int -> Int -> IO [Int]
rolls n s = sequence . replicate n . randomRIO $ (1,s)

roll :: DiceExp -> IO Int
roll de =
  case de of
    Sum e1 e2 -> (+) <$> roll e1 <*> roll e2
    Const n   -> return n
    Die n s   -> sum <$> rolls n s

main :: IO ()
main = do
  args <- fmap concat getArgs
  let parseFail  = putStrLn $ "Could not parse \"" ++ args ++ "\" as dice expression!"
      doRoll exp = roll exp >>= (putStrLn . show)
  maybe parseFail doRoll (parse args)
