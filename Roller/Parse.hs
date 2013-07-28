module Roller.Parse (parse) where

import Roller.Types

import Prelude hiding (const, sum)
import Text.Regex.Applicative hiding (Const)
import Data.Char (isDigit, isSpace)

type Parser a = RE Char a

int :: Parser Int
int = read <$> some oneDigit
  where oneDigit = psym isDigit

die :: Parser DiceExp
die = Die <$> int <* sym 'd' <*> int

const :: Parser DiceExp
const = Const <$> int

sum' :: Parser DiceExp
sum' = Sum <$> die <* sym '+' <*> const

diceExp' :: Parser DiceExp
diceExp' = sum' <|> die <|> const

addend :: Parser DiceExp
addend = die <|> const

diceExp :: Parser DiceExp
diceExp = (collect <$> addend <*> many sum1) <|> addend
  where sum1 :: Parser (DiceExp -> DiceExp)
        sum1        = flip Sum <$> plusAnother
        plusAnother = sym '+' *> addend
        collect     = foldl . flip $ ($)

parse :: String -> Maybe DiceExp
parse s = let s' = filter (not . isSpace) s in s' =~ diceExp

