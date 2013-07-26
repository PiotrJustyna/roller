module Roller.Parse (parse) where

import Roller.Types

import Prelude hiding (const, sum)
import Text.Regex.Applicative hiding (Const)
import Data.Char (isDigit)

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
