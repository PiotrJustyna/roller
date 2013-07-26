module Roller.Types where

data DiceExp =
     Die Int Int
   | Const Int
   | Sum DiceExp DiceExp

instance Show DiceExp where
  show (Die n s)   = show n ++ "d" ++ show s
  show (Const n)   = show n
  show (Sum e1 e2) = show e1 ++ "+" ++ show e2

