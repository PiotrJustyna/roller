import Roller.Types
import Roller.Parse

import Data.Char
import Data.Word
import Test.QuickCheck
import Text.Regex.Applicative

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

  print $ "Verify parse natural number given numerical text."
  quickCheck prop_ParseNaturalNumberGivenNumericalText

  print $ "Verify parse natural number given nonnumerical text."
  quickCheck prop_ParseNaturalNumberGivenNonnumericalText

  print $ "Verify parse natural number given mixed text."
  quickCheck prop_ParseNaturalNumberGivenMixedText

prop_ShowDieTerm :: Word8 -> Word8 -> Bool
prop_ShowDieTerm x y = show (DieTerm x y) == show x ++ show dieSymbol ++ show y

prop_ShowAddedDieTerm :: Word8 -> Word8 -> Bool
prop_ShowAddedDieTerm x y = show (AddedDieTerm x y) == show additionSymbol ++ show x ++ show dieSymbol ++ show y

prop_ShowSubtractedDieTerm :: Word8 -> Word8 -> Bool
prop_ShowSubtractedDieTerm x y = show (SubtractedDieTerm x y) == show subtractionSymbol ++ show x ++ show dieSymbol ++ show y

prop_ShowConstantTerm :: Word8 -> Bool
prop_ShowConstantTerm x = show (ConstantTerm x) == show x

prop_ShowAddedConstantTerm :: Word8 -> Bool
prop_ShowAddedConstantTerm x = show (AddedConstantTerm x) == show additionSymbol ++ show x

prop_ShowSubtractedConstantTerm :: Word8 -> Bool
prop_ShowSubtractedConstantTerm x = show (SubtractedConstantTerm x) == show subtractionSymbol ++ show x

newtype NumericalTextGenerator = NumericalTextGenerator String deriving Show
newtype NonNumericalTextGenerator = NonNumericalTextGenerator String deriving Show

instance Arbitrary NumericalTextGenerator where arbitrary = NumericalTextGenerator <$> generateNumberString

generateDigit :: Gen Char
generateDigit = elements ['0' .. '9']

generateNumberString :: Gen String
generateNumberString = listOf generateDigit

prop_ParseNaturalNumberGivenNumericalText :: NumericalTextGenerator -> Bool
prop_ParseNaturalNumberGivenNumericalText (NumericalTextGenerator x) =
  case (x =~ naturalNumber) of
    Just naturalNumber -> if naturalNumber >= 0 then True else False  -- input: numerical text
    Nothing -> True                                                   -- input: empty text

prop_ParseNaturalNumberGivenNonnumericalText :: String -> Property
prop_ParseNaturalNumberGivenNonnumericalText x =
  containsNoDigits x ==>
  case (x =~ naturalNumber) of
    Just naturalNumber -> False -- input: nonnumerical text, only Nothing expected
    Nothing -> True             -- input: empty text
  where
    containsNoDigits x = (foldl (\y z -> if y == 0 && z `elem` x then y + 1 else y) 0 ['0' .. '9']) == 0

prop_ParseNaturalNumberGivenMixedText :: String -> Bool
prop_ParseNaturalNumberGivenMixedText x =
  case (x =~ naturalNumber) of
    Just naturalNumber -> if naturalNumber >= 0 then True else False  -- input: random, but numerical/parsable text
    Nothing -> True                                                   -- input: random, non-parsable text
