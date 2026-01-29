module ClosureConvert.SourceParser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import ClosureConvert.Source

-- Parser for source language
type Parser = Parsec Void String

-- Space consumer that handles whitespace and comments
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Parse integers
integer :: Parser Int
integer = lexeme L.decimal

-- Parse identifiers
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar) >>= check
  where
    check x = if x `elem` keywords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

keywords :: [String]
keywords = ["let", "in", "if", "then", "else", "true", "false"]

-- Reserved words
reserved :: String -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

-- Parse parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parse expressions
exprParser :: Parser Expr
exprParser = letParser <|> lamParser <|> ifParser <|> addParser

-- Let expressions with multiple bindings
letParser :: Parser Expr
letParser = do
  reserved "let"
  bindings <- bindingParser `sepBy1` symbol ","
  reserved "in"
  body <- exprParser
  return $ Let bindings body

bindingParser :: Parser (String, Expr)
bindingParser = do
  name <- identifier
  _ <- symbol "="
  expr <- exprParser
  return (name, expr)

-- Lambda expressions
lamParser :: Parser Expr
lamParser = do
  _ <- symbol "\\" <|> symbol "λ"
  param <- identifier
  _ <- symbol "."
  body <- exprParser
  return $ Lam param body

-- If expressions
ifParser :: Parser Expr
ifParser = do
  reserved "if"
  cond <- exprParser
  reserved "then"
  thenBranch <- exprParser
  reserved "else"
  elseBranch <- exprParser
  return $ If cond thenBranch elseBranch

-- Addition (left-associative)
addParser :: Parser Expr
addParser = do
  first <- appParser
  rest <- many ((symbol "+" *> appParser))
  return $ foldl Add first rest

-- Application (left-associative)
appParser :: Parser Expr
appParser = do
  first <- atomParser
  rest <- many atomParser
  return $ foldl App first rest

-- Atomic expressions
atomParser :: Parser Expr
atomParser = choice
  [ IntLit <$> integer
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , Var <$> identifier
  , parens exprParser
  ]

-- Top-level parser
parseExpr :: String -> Either String Expr
parseExpr input = case runParser (sc *> exprParser <* eof) "" input of
  Left err -> Left (errorBundlePretty err)
  Right expr -> Right expr

