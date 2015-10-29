module Roller.Core (main) where

import Roller.Types
import Roller.Parse
import Roller.CLI

import System.Environment (getArgs)
import System.Random (randomRIO)
import Control.Applicative
import Control.Monad (join, replicateM, replicateM_)
import Data.Word

positiveRoll :: Word8 -> IO Integer
positiveRoll x = randomRIO $ (1, fromIntegral x)

negativeRoll :: Word8 -> IO Integer
negativeRoll x = (*(-1)) <$> positiveRoll x

positiveRolls :: Word8 -> Word8 -> IO [Integer]
positiveRolls x y = replicateM (fromIntegral x) . positiveRoll $ y

negativeRolls :: Word8 -> Word8 -> IO [Integer]
negativeRolls x y = replicateM (fromIntegral x) . negativeRoll $ y

rolls :: [DiceExpression] -> IO [[Integer]]
rolls expressions = foldl (\x y -> (++) <$> x <*> (extractDiceExpressionValue y)) (pure [[]]) expressions

extractDiceExpressionValue :: DiceExpression -> IO [[Integer]]
extractDiceExpressionValue expression =
  case expression of
    DieTerm x y -> return <$> positiveRolls x y
    AddedDieTerm x y -> return <$> positiveRolls x y
    SubtractedDieTerm x y -> return <$> negativeRolls x y
    ConstantTerm x -> return [[fromIntegral x]]
    AddedConstantTerm x -> return [[fromIntegral x]]
    SubtractedConstantTerm x -> return [[(-1) * (fromIntegral x)]]

rollEm :: CLI (IO ())
rollEm verbose n args = maybe parseFail rollMany (parse input)
  where
    input        = concat args
    rollMany     = replicateM_ n . rollOnce
    rollOnce exp = fmap summary (rolls exp) >>= putStrLn

    summary      = if verbose then show else show . sumRolls
    sumRolls     = sum . map sum
    parseFail    = putStrLn $ "Could not parse \"" ++ input ++ "\" as dice expression!"

main :: IO ()
main = join . withOpts $ rollEm
