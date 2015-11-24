import Roller.Types
import Roller.Parse

import Data.Char
import Test.QuickCheck
import Text.Regex.Applicative

newtype NumericalTextGenerator = NumericalTextGenerator String deriving Show
newtype CorrectDieTermGenerator = CorrectDieTermGenerator String deriving Show

instance Arbitrary NumericalTextGenerator where arbitrary = NumericalTextGenerator <$> generateNumberText
instance Arbitrary CorrectDieTermGenerator where arbitrary = CorrectDieTermGenerator <$> generateCorrectDieTerm

main :: IO ()
main = do
	print $ "Verify parse natural number given numerical text."
	quickCheck prop_ParseNaturalNumberGivenNumericalText

	print $ "Verify parse natural number given nonnumerical text."
	quickCheck prop_ParseNaturalNumberGivenNonnumericalText

	print $ "Verify parse Die Term given correct die term text."
	quickCheck prop_ParseDieTermGivenCorrectDieTermText

generateDigit :: Gen Char
generateDigit = elements ['0' .. '9']

generateNumberText :: Gen String
generateNumberText = listOf1 generateDigit

generateCorrectNumberOfDiceText :: Gen String
generateCorrectNumberOfDiceText = resize 2 $ listOf1 generateDigit

generateCorrectNumberOfFacesOfEachDieText :: Gen String
generateCorrectNumberOfFacesOfEachDieText = resize 2 $ listOf1 generateDigit

generateCorrectDieTerm :: Gen String
generateCorrectDieTerm = (++) <$> generateCorrectNumberOfDiceText <*> ((:) <$> pure 'd' <*> generateCorrectNumberOfFacesOfEachDieText)

prop_ParseNaturalNumberGivenNumericalText :: NumericalTextGenerator -> Bool
prop_ParseNaturalNumberGivenNumericalText (NumericalTextGenerator text) =
  case (text =~ naturalNumber) of
    Just x -> if x >= 0 then True else False
    Nothing -> False

prop_ParseNaturalNumberGivenNonnumericalText :: String -> Property
prop_ParseNaturalNumberGivenNonnumericalText text =
  containsNoDigits text ==>
  case (text =~ naturalNumber) of
    Just x -> False
    Nothing -> True
  where
    containsNoDigits x = (foldl (\y z -> if y == 0 && z `elem` x then y + 1 else y) 0 ['0' .. '9']) == 0

prop_ParseDieTermGivenCorrectDieTermText :: CorrectDieTermGenerator -> Property
prop_ParseDieTermGivenCorrectDieTermText (CorrectDieTermGenerator text) =
  collect text $
	collect (show (text =~ dieTerm)) $
	True ==>
  case (text =~ dieTerm) of
    Just (DieTerm x y) -> True
    Nothing -> False
