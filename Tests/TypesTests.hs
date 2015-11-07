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

  print $ "Verify parse NaturalNumber"
  quickCheck prop_ParseNaturalNumber

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

prop_ParseNaturalNumber :: String -> Property
prop_ParseNaturalNumber x =
  classify (containsOnlyDigits x) "tests contain only digits." $
  classify (not $ containsOnlyDigits x) "tests contain not only digits." $
  case (x =~ naturalNumber) of
    Just naturalNumber -> if naturalNumber > 0 then True else False
    Nothing -> True
  where
    containsOnlyDigits x =
      length x > 0
      && (foldl (\y z -> if isDigit z then y + 1 else y) 0 x) == length x
