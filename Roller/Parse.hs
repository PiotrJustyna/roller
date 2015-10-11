module Roller.Parse (parse) where

import Roller.Types

import Prelude hiding (const, sum)
import Text.Regex.Applicative
import Data.Char (isDigit, isSpace)
import Data.Word

type Parser a = RE Char a

naturalNumber :: Parser Word8
naturalNumber = read <$> some oneDigit
  where oneDigit = psym isDigit

die :: Parser DiceExpression
die = Die <$> naturalNumber <* sym 'd' <*> naturalNumber

const :: Parser DiceExpression
const = Constant <$> naturalNumber

diceExpression :: Parser DiceExpression
diceExpression = foldl1 Sum <$> terms
  where terms = (:) <$> term <*> many (sym '+' *> term)
        term  = die <|> const

parse :: String -> Maybe DiceExpression
parse s = let s' = filter (not . isSpace) s in s' =~ diceExpression
