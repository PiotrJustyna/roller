import Roller.Types

import Test.QuickCheck
import Data.Word

main :: IO ()
main = do
  print $ "Verify show DieTerm."
  quickCheck verifyShowDieTerm

  print $ "Verify show AddedDieTerm."
  quickCheck verifyShowAddedDieTerm

  print $ "Verify show SubtractedDieTerm."
  quickCheck verifyShowSubtractedDieTerm

  print $ "Verify show ConstantTerm."
  quickCheck verifyShowConstantTerm

  print $ "Verify show AddedConstantTerm."
  quickCheck verifyShowAddedConstantTerm

  print $ "Verify show SubtractedConstantTerm."
  quickCheck verifyShowSubtractedConstantTerm

verifyShowDieTerm :: Word8 -> Word8 -> Bool
verifyShowDieTerm x y = show (DieTerm x y) == show x ++ show dieSymbol ++ show y

verifyShowAddedDieTerm :: Word8 -> Word8 -> Bool
verifyShowAddedDieTerm x y = show (AddedDieTerm x y) == show additionSymbol ++ show x ++ show dieSymbol ++ show y

verifyShowSubtractedDieTerm :: Word8 -> Word8 -> Bool
verifyShowSubtractedDieTerm x y = show (SubtractedDieTerm x y) == show subtractionSymbol ++ show x ++ show dieSymbol ++ show y

verifyShowConstantTerm :: Word8 -> Bool
verifyShowConstantTerm x = show (ConstantTerm x) == show x

verifyShowAddedConstantTerm :: Word8 -> Bool
verifyShowAddedConstantTerm x = show (AddedConstantTerm x) == show additionSymbol ++ show x

verifyShowSubtractedConstantTerm :: Word8 -> Bool
verifyShowSubtractedConstantTerm x = show (SubtractedConstantTerm x) == show subtractionSymbol ++ show x

-- build with:
-- ghc -package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d Tests/TypesTests.hs
