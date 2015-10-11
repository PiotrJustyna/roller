-- Dice notation taken from:
-- https://en.wikipedia.org/wiki/Dice_notation

module Roller.Types where

import Data.Word

type NumberOfDice = Word8
type NumberOfFacesOfEachDie = Word8

data DiceExpression =
     Die NumberOfDice NumberOfFacesOfEachDie
   | Constant Word8
   | Sum DiceExpression DiceExpression

instance Show DiceExpression where
  show (Die x y) = show x ++ "d" ++ show y
  show (Constant x) = show x
  show (Sum x y) = show x ++ "+" ++ show y
