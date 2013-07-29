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

diceExp :: Parser DiceExp
diceExp = foldl1 Sum <$> terms
  where terms = (:) <$> term <*> many (sym '+' *> term)
        term  = die <|> const

parse :: String -> Maybe DiceExp
parse s = let s' = filter (not . isSpace) s in s' =~ diceExp

