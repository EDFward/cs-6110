module Parser
  ( parse
  ) where

import           Control.Applicative
import           Data.Char                    (isAsciiLower)
import           Data.List                    (find)
import           Text.ParserCombinators.ReadP

import           Expr

-- Only take one lowercase letter.
parseName :: ReadP Name
parseName = do
  n <- satisfy isAsciiLower
  return [n]

parseVar :: ReadP Expr
parseVar = do
  n <- parseName
  return $ Var n

parseLam :: ReadP Expr
parseLam = do
  string "λ"
  n <- parseName
  string "."
  body <- parseExpr
  return $ Lam n body

brackets :: ReadP a -> ReadP a
brackets p = do
  char '('
  r <- p
  char ')'
  return r

parseTerm :: ReadP Expr
parseTerm = parseVar +++ parseLam +++ brackets parseExpr

parseExpr :: ReadP Expr
parseExpr = do
  terms <- many1 parseTerm
  return $ foldl1 App terms

parse :: String -> Maybe Expr
parse s = case readP_to_S parseExpr s of
  [] -> Nothing
  ls -> case last ls of
    -- Regard as failure if having more chars to consume.
    (e, []) -> Just e
    _       -> Nothing
