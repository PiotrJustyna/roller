module Roller.Core where

import Prelude hiding (const, sum)
import Text.Regex.Applicative hiding (Const)
import Data.Char (isDigit)

data DiceExp =
     Die Int Int
   | Const Int
   | Sum DiceExp DiceExp

instance Show DiceExp where
  show (Die n s)   = show n ++ "d" ++ show s
  show (Const n)   = show n
  show (Sum e1 e2) = show e1 ++ "+" ++ show e2

type Parser a = RE Char a

int :: Parser Int
int = read <$> some oneDigit
  where oneDigit = psym isDigit

die :: Parser DiceExp
die = Die <$> int <* sym 'd' <*> int

const :: Parser DiceExp
const = Const <$> int

sum :: Parser DiceExp
sum = Sum <$> die <* sym '+' <*> const

diceExp :: Parser DiceExp
diceExp = sum <|> die <|> const

parse :: String -> Maybe DiceExp
parse = (=~ diceExp)
