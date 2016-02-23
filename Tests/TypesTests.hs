import Roller.Types

import Control.Applicative
import Data.Word
import Test.QuickCheck

main :: IO ()
main = do
  print $ "Verify show DieTerm."
  quickCheck prop_ShowDieTerm

  print $ "Verify show AddedDieTerm."
  quickCheck prop_ShowAddedDieTerm

  print $ "Verify show SubtractedDieTerm."
  quickCheck prop_ShowSubtractedDieTerm

  print $ "Verify show ConstantTerm."
  quickCheck prop_ShowConstantTerm

  print $ "Verify show AddedConstantTerm."
  quickCheck prop_ShowAddedConstantTerm

  print $ "Verify show SubtractedConstantTerm."
  quickCheck prop_ShowSubtractedConstantTerm

prop_ShowDieTerm :: CorrectNumberOfDiceGenerator -> CorrectNumberOfFacesOfEachDieGenerator -> Bool
prop_ShowDieTerm (CorrectNumberOfDiceGenerator x) (CorrectNumberOfFacesOfEachDieGenerator y) =
  show (constructDieTerm x y) == show x ++ show dieSymbol ++ show y

prop_ShowAddedDieTerm :: CorrectNumberOfDiceGenerator -> CorrectNumberOfFacesOfEachDieGenerator -> Bool
prop_ShowAddedDieTerm (CorrectNumberOfDiceGenerator x) (CorrectNumberOfFacesOfEachDieGenerator y) =
  show (constructAddedDieTerm x y) == show additionSymbol ++ show x ++ show dieSymbol ++ show y

prop_ShowSubtractedDieTerm :: CorrectNumberOfDiceGenerator -> CorrectNumberOfFacesOfEachDieGenerator -> Bool
prop_ShowSubtractedDieTerm (CorrectNumberOfDiceGenerator x) (CorrectNumberOfFacesOfEachDieGenerator y) =
  show (constructSubtractedDieTerm x y) == show subtractionSymbol ++ show x ++ show dieSymbol ++ show y

prop_ShowConstantTerm :: CorrectConstatntGenerator -> Bool
prop_ShowConstantTerm (CorrectConstatntGenerator x) = show (constructConstantTerm x) == show x

prop_ShowAddedConstantTerm :: CorrectConstatntGenerator -> Bool
prop_ShowAddedConstantTerm (CorrectConstatntGenerator x) = show (constructAddedConstantTerm x) == show additionSymbol ++ show x

prop_ShowSubtractedConstantTerm :: CorrectConstatntGenerator -> Bool
prop_ShowSubtractedConstantTerm (CorrectConstatntGenerator x) = show (constructSubtractedConstantTerm x) == show subtractionSymbol ++ show x

newtype CorrectNumberOfDiceGenerator = CorrectNumberOfDiceGenerator NumberOfDice deriving Show
instance Arbitrary CorrectNumberOfDiceGenerator where arbitrary = CorrectNumberOfDiceGenerator <$> generateCorrectNumberOfDice

generateCorrectNumberOfDice :: Gen Word8
generateCorrectNumberOfDice = elements [0 .. 99]

newtype CorrectNumberOfFacesOfEachDieGenerator = CorrectNumberOfFacesOfEachDieGenerator NumberOfFacesOfEachDie deriving Show
instance Arbitrary CorrectNumberOfFacesOfEachDieGenerator where arbitrary = CorrectNumberOfFacesOfEachDieGenerator <$> generateCorrectNumberOfFacesOfEachDie

generateCorrectNumberOfFacesOfEachDie :: Gen Word8
generateCorrectNumberOfFacesOfEachDie = elements [0 .. 99]

newtype CorrectConstatntGenerator = CorrectConstatntGenerator Word8 deriving Show
instance Arbitrary CorrectConstatntGenerator where arbitrary = CorrectConstatntGenerator <$> generateCorrectConstant

generateCorrectConstant :: Gen Word8
generateCorrectConstant = elements [0 .. 99]
