module Main where

import qualified Roller.Core as Roller

main :: IO ()
main = Roller.main

-- Build with:
-- ghc -package-db=.cabal-sandbox\x86_64-windows-ghc-7.10.2-packages.conf.d Roller.hs
