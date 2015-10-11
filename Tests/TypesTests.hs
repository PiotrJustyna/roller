import Roller.Types

import Test.QuickCheck
import Data.Word

main :: IO ()
main = do
    print $ "Verify show Die."
    quickCheck verifyShowDie

    print $ "Verify show Const."
    quickCheck verifyShowConst

    print $ "Verify show Sum."
    quickCheck verifyShowSum

verifyShowDie :: Word8 -> Word8 -> Bool
verifyShowDie x y = show (Die x y) == show x ++ "d" ++ show y

verifyShowConst :: Word8 -> Bool
verifyShowConst x = show (Constant x) == show x

verifyShowSum :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
verifyShowSum w x y z = show (Sum (Die w x) (Die y z)) == show (Die w x) ++ "+" ++ show (Die y z)

-- build with:
-- ghc -package-db=.cabal-sandbox\x86_64-windows-ghc-7.10.2-packages.conf.d Tests\TypesTests.hs
