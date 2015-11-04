module Main where

import qualified Roller.Core as Roller

main :: IO ()
main = Roller.main

-- Build with:
--
-- Windows:
-- ghc -package-db=.cabal-sandbox\x86_64-windows-ghc-7.10.2-packages.conf.d Roller.hs
--
-- Linux:
-- ghc -package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d Roller.hs -o roller 
