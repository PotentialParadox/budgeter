module Budget.Parse(
  getBudgetInfo
  )where

import Text.Parsec
import Text.Parsec.String
import Data.List
import Budget.Types

getBudgetInfo :: Parser BudgetInfo
getBudgetInfo = do
  income <- header
  saved <- header
  ytr <- header
  apr <- header
  newline
  many1 (noneOf "\n")
  newline
  items <- budgetItem `sepEndBy` (char ',')
  return $ BudgetInfo income saved ytr apr items

header :: Parser Double
header = do
  many1 (noneOf "=")
  char '='
  spaces
  value <- many1 (digit <|> char '.' <|> char '-')
  return $ read value

budgetItem :: Parser BudgetItem
budgetItem = do
  spaces
  bName <- nameWords
  spaces
  bImportance <- many1 digit
  spaces
  bMinPrice <- many1 digit
  spaces
  bMaxPrice <- many1 digit
  spaces
  bNeeded <- char '0' <|> char '1'
  spaces
  return $ BudgetItem bName (read bImportance) (read bMinPrice) (read bMaxPrice) (toBool bNeeded)

nameWords :: Parser String
nameWords = do
  spaces
  word1s <- wordItem `sepBy` spaces
  return $ concat $ intersperse " " word1s

wordItem :: Parser String
wordItem = many1 (letter <|> char '(' <|> char ')') <* spaces

toBool :: Char -> Bool
toBool c =
  case c of
    '0' -> False
    '1' -> True
    _ -> False
