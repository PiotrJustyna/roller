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
dieTerm = constructDieTerm <$> naturalNumber <* sym dieSymbol <*> naturalNumber

addedDieTerm :: RE Char DiceExpression
addedDieTerm = constructAddedDieTerm <$> (sym additionSymbol *> naturalNumber) <* sym dieSymbol <*> naturalNumber

subtractedDieTerm :: RE Char DiceExpression
subtractedDieTerm = constructSubtractedDieTerm <$> (sym subtractionSymbol *> naturalNumber) <* sym dieSymbol <*> naturalNumber

constantTerm :: RE Char DiceExpression
constantTerm = constructConstantTerm <$> naturalNumber

addedConstantTerm :: RE Char DiceExpression
addedConstantTerm = constructAddedConstantTerm <$> (sym additionSymbol *> naturalNumber)

subtractedConstantTerm :: RE Char DiceExpression
subtractedConstantTerm = constructSubtractedConstantTerm <$> (sym subtractionSymbol *> naturalNumber)

diceExpression :: RE Char [DiceExpression]
diceExpression = (:) <$> term <*> many signedTerm where
  term = dieTerm <|> constantTerm
  signedTerm = signedDieTerm <|> signedConstantTerm
  signedDieTerm = addedDieTerm <|> subtractedDieTerm
  signedConstantTerm = addedConstantTerm <|> subtractedConstantTerm

parse :: String -> Maybe [DiceExpression]
parse x = filter (not . isSpace) x =~ diceExpression
