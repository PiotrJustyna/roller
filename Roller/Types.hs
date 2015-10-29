-- Dice notation taken from:
-- https://en.wikipedia.org/wiki/Dice_notation

module Roller.Types where

import Data.Word

type NumberOfDice = Word8
type NumberOfFacesOfEachDie = Word8

dieSymbol = 'd'
additionSymbol = '+'
subtractionSymbol = '-'

data DiceExpression =
    DieTerm NumberOfDice NumberOfFacesOfEachDie
    | AddedDieTerm NumberOfDice NumberOfFacesOfEachDie
    | SubtractedDieTerm NumberOfDice NumberOfFacesOfEachDie
    | ConstantTerm Word8
    | AddedConstantTerm Word8
    | SubtractedConstantTerm Word8

instance Show DiceExpression where
    show (DieTerm x y) = show x ++ show dieSymbol ++ show y
    show (AddedDieTerm x y) = show additionSymbol ++ show x ++ show dieSymbol ++ show y
    show (SubtractedDieTerm x y) = show subtractionSymbol ++ show x ++ show dieSymbol ++ show y
    show (ConstantTerm x) = show x
    show (AddedConstantTerm x) = show additionSymbol ++ show x
    show (SubtractedConstantTerm x ) = show subtractionSymbol ++ show x
