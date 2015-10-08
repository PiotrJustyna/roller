import Roller.Types
import Test.QuickCheck

main :: IO ()
main = do
    print $ "Verify show Die."
    quickCheck verifyShowDie

    print $ "Verify show Const."
    quickCheck verifyShowConst

    print $ "Verify show Sum."
    quickCheck verifyShowSum

verifyShowDie :: Int -> Int -> Bool
verifyShowDie x y = show (Die x y) == show x ++ "d" ++ show y

verifyShowConst :: Int -> Bool
verifyShowConst x = show (Const x) == show x

verifyShowSum :: Int -> Int -> Int -> Int -> Bool
verifyShowSum w x y z = show (Sum (Die w x) (Die y z)) == show (Die w x) ++ "+" ++ show (Die y z)

-- build with:
-- ghc -package-db=.cabal-sandbox\x86_64-windows-ghc-7.10.2-packages.conf.d Tests\TypesTests.hs
