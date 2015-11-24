module Roller.Parse
(
  parse,
  naturalNumber,
  dieTerm
) where

import Roller.Types

import Text.Regex.Applicative
import Data.Char (isDigit, isSpace)
import Data.Word

naturalNumber :: RE Char Word8
naturalNumber = read <$> some (psym isDigit)

dieTerm :: RE Char DiceExpression
dieTerm = DieTerm <$> naturalNumber <* sym dieSymbol <*> naturalNumber

addedDieTerm :: RE Char DiceExpression
addedDieTerm = AddedDieTerm <$> (sym additionSymbol *> naturalNumber) <* sym dieSymbol <*> naturalNumber

subtractedDieTerm :: RE Char DiceExpression
subtractedDieTerm = SubtractedDieTerm <$> (sym subtractionSymbol *> naturalNumber) <* sym dieSymbol <*> naturalNumber

constantTerm :: RE Char DiceExpression
constantTerm = ConstantTerm <$> naturalNumber

addedConstantTerm :: RE Char DiceExpression
addedConstantTerm = AddedConstantTerm <$> (sym additionSymbol *> naturalNumber)

subtractedConstantTerm :: RE Char DiceExpression
subtractedConstantTerm = SubtractedConstantTerm <$> (sym subtractionSymbol *> naturalNumber)

diceExpression :: RE Char [DiceExpression]
diceExpression = (:) <$> term <*> many signedTerm where
  term = dieTerm <|> constantTerm
  signedTerm = signedDieTerm <|> signedConstantTerm
  signedDieTerm = addedDieTerm <|> subtractedDieTerm
  signedConstantTerm = addedConstantTerm <|> subtractedConstantTerm

parse :: String -> Maybe [DiceExpression]
parse x = filter (not . isSpace) x =~ diceExpression
