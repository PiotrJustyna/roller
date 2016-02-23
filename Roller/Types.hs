-- Dice notation:
-- https://en.wikipedia.org/wiki/Dice_notation

module Roller.Types (
    NumberOfDice,
    NumberOfFacesOfEachDie,
    DiceExpression,
    maybeDiceExpressionDestructor,
    constructDieTerm,
    constructAddedDieTerm,
    constructSubtractedDieTerm,
    constructConstantTerm,
    constructAddedConstantTerm,
    constructSubtractedConstantTerm,
    dieSymbol,
    additionSymbol,
    subtractionSymbol,
    rolls
  ) where

import Control.Applicative
import Control.Monad (join, replicateM)
import Data.Word
import System.Random (randomRIO)

type NumberOfDice = Word8
type NumberOfFacesOfEachDie = Word8

dieSymbol = 'd'
additionSymbol = '+'
subtractionSymbol = '-'
diceLimit = 99
facesOfEachDieLimit = 99
constantLimit = 99

data DiceExpression =
    DieTerm NumberOfDice NumberOfFacesOfEachDie
    | AddedDieTerm NumberOfDice NumberOfFacesOfEachDie
    | SubtractedDieTerm NumberOfDice NumberOfFacesOfEachDie
    | ConstantTerm Word8
    | AddedConstantTerm Word8
    | SubtractedConstantTerm Word8

diceExpressionDestructor :: (NumberOfDice -> NumberOfFacesOfEachDie -> Bool) -> (NumberOfDice -> NumberOfFacesOfEachDie -> Bool) -> (NumberOfDice -> NumberOfFacesOfEachDie -> Bool) -> (Word8 -> Bool) -> (Word8 -> Bool) -> (Word8 -> Bool) -> DiceExpression -> Bool
diceExpressionDestructor dieTerm addedDieTerm subtractedDieTerm constantTerm addedConstantTerm subtractedConstantTerm x = case x of
  DieTerm numberOfDice numberOfFacesOfEachDie -> dieTerm numberOfDice numberOfFacesOfEachDie
  AddedDieTerm numberOfDice numberOfFacesOfEachDie -> addedDieTerm numberOfDice numberOfFacesOfEachDie
  SubtractedDieTerm numberOfDice numberOfFacesOfEachDie -> subtractedDieTerm numberOfDice numberOfFacesOfEachDie
  ConstantTerm n -> constantTerm n
  AddedConstantTerm n -> addedConstantTerm n
  SubtractedConstantTerm n -> subtractedConstantTerm n

maybeDiceExpressionDestructor :: (NumberOfDice -> NumberOfFacesOfEachDie -> Bool) -> (NumberOfDice -> NumberOfFacesOfEachDie -> Bool) -> (NumberOfDice -> NumberOfFacesOfEachDie -> Bool) -> (Word8 -> Bool) -> (Word8 -> Bool) -> (Word8 -> Bool) -> Bool-> Maybe DiceExpression -> Bool
maybeDiceExpressionDestructor dieTerm addedDieTerm subtractedDieTerm constantTerm addedConstantTerm subtractedConstantTerm nothingResult x = case x of
  Just diceExpression -> diceExpressionDestructor dieTerm addedDieTerm subtractedDieTerm constantTerm addedConstantTerm subtractedConstantTerm diceExpression
  Nothing -> nothingResult

instance Show DiceExpression where
    show (DieTerm x y) = show x ++ show dieSymbol ++ show y
    show (AddedDieTerm x y) = show additionSymbol ++ show x ++ show dieSymbol ++ show y
    show (SubtractedDieTerm x y) = show subtractionSymbol ++ show x ++ show dieSymbol ++ show y
    show (ConstantTerm x) = show x
    show (AddedConstantTerm x) = show additionSymbol ++ show x
    show (SubtractedConstantTerm x ) = show subtractionSymbol ++ show x

constructDieTerm :: NumberOfDice -> NumberOfFacesOfEachDie -> DiceExpression
constructDieTerm x y
  | validateDieTermParameters x y = DieTerm x y
  | otherwise = error $ dieTermLimitsErrorMessage x y

constructAddedDieTerm :: NumberOfDice -> NumberOfFacesOfEachDie -> DiceExpression
constructAddedDieTerm x y
  | validateDieTermParameters x y = AddedDieTerm x y
  | otherwise = error $ dieTermLimitsErrorMessage x y

constructSubtractedDieTerm :: NumberOfDice -> NumberOfFacesOfEachDie -> DiceExpression
constructSubtractedDieTerm x y
  | validateDieTermParameters x y = SubtractedDieTerm x y
  | otherwise = error $ dieTermLimitsErrorMessage x y

constructConstantTerm :: Word8 -> DiceExpression
constructConstantTerm x
  | validateConstantTermParameter x = ConstantTerm x
  | otherwise = error $ constantTermLimitErrorMessage x

constructAddedConstantTerm :: Word8 -> DiceExpression
constructAddedConstantTerm x
  | validateConstantTermParameter x = AddedConstantTerm x
  | otherwise = error $ constantTermLimitErrorMessage x

constructSubtractedConstantTerm :: Word8 -> DiceExpression
constructSubtractedConstantTerm x
  | validateConstantTermParameter x = SubtractedConstantTerm x
  | otherwise = error $ constantTermLimitErrorMessage x

validateDieTermParameters :: NumberOfDice -> NumberOfFacesOfEachDie -> Bool
validateDieTermParameters x y = x <= diceLimit && y <= facesOfEachDieLimit

validateConstantTermParameter :: Word8 -> Bool
validateConstantTermParameter x = x <= constantLimit

dieTermLimitsErrorMessage :: NumberOfDice -> NumberOfFacesOfEachDie -> String
dieTermLimitsErrorMessage x y =
  "Number of dice or number of faces of each die incorrect.\n"
  ++ "Details:\n"
  ++ "Given number of dice: " ++ show x ++ " (limit: " ++ show diceLimit ++ ").\n"
  ++ "Given number of faces of each die: " ++ show y ++ " (limit: " ++ show facesOfEachDieLimit ++ ")."

constantTermLimitErrorMessage :: Word8 -> String
constantTermLimitErrorMessage x =
  "Constat incorrect.\n"
  ++ "Details:\n"
  ++ "Given constant: " ++ show x ++ " (limit: " ++ show constantLimit ++ ")."

positiveRoll :: Word8 -> IO Integer
positiveRoll x = randomRIO $ (1, fromIntegral x)

negativeRoll :: Word8 -> IO Integer
negativeRoll x = (*(-1)) <$> positiveRoll x

positiveRolls :: Word8 -> Word8 -> IO [Integer]
positiveRolls x y = replicateM (fromIntegral x) . positiveRoll $ y

negativeRolls :: Word8 -> Word8 -> IO [Integer]
negativeRolls x y = replicateM (fromIntegral x) . negativeRoll $ y

rolls :: [DiceExpression] -> IO [Integer]
rolls expressions = foldl (\x y -> (++) <$> x <*> (extractDiceExpressionValue y)) (pure []) expressions

extractDiceExpressionValue :: DiceExpression -> IO [Integer]
extractDiceExpressionValue expression =
  case expression of
    DieTerm x y -> positiveRolls x y
    AddedDieTerm x y -> positiveRolls x y
    SubtractedDieTerm x y -> negativeRolls x y
    ConstantTerm x -> return [fromIntegral x]
    AddedConstantTerm x -> return [fromIntegral x]
    SubtractedConstantTerm x -> return [(-1) * (fromIntegral x)]
