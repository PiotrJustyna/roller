module Roller.Parse (parse) where

import Roller.Types

import Prelude hiding (const, sum)
import Text.Regex.Applicative hiding (Const)
import Data.Char (isDigit, isSpace)

type Parser a = RE Char a

int :: Parser Int
int = read <$> some oneDigit
  where oneDigit = psym isDigit

die :: Parser DiceExpression
die = Die <$> int <* sym 'd' <*> int

const :: Parser DiceExpression
const = Const <$> int

diceExpression :: Parser DiceExpression
diceExpression = foldl1 Sum <$> terms
  where terms = (:) <$> term <*> many (sym '+' *> term)
        term  = die <|> const

parse :: String -> Maybe DiceExpression
parse s = let s' = filter (not . isSpace) s in s' =~ diceExpression
