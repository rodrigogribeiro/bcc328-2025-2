module Parsing.Descendent.ExpExample ( exprLexer
                                     , lexer'
                                     , runParser
                                     , exprParser
                                     , expr
                                     ) where

import Parsing.Descendent.Combinators
import Parsing.Descendent.Expr

-- expressions

data Expr
  = Var String
  | Lit Int
  | Expr :+: Expr
  | Expr :*: Expr
  deriving (Eq, Show)


-- lexical analyser functions

exprLexer :: String -> [Token]
exprLexer s
  = case runParser lexer' s of
      [] -> []
      ((x, "" ):_) -> x
      v -> error $ show v


lexer' :: Parser Char [Token]
lexer' = (\_ x -> x) <$> spaces 
                        <*> many ((\x _ -> x) <$> tokenLexer <*> spaces)

-- tokens

data Token
  = Id String
  | Number Int
  | Add
  | Mult
  | LParen
  | RParen
  deriving (Eq, Show)


tokenLexer :: Parser Char Token
tokenLexer = (Id <$> identifier) <|> number <|> tokenlex

number :: Parser Char Token
number = Number <$> natural

-- a table for the lexical analyser

table :: [(Char, Token)]
table = [('+', Add), ('*', Mult),
         ('(', LParen), (')', RParen)]

tokenlex :: Parser Char Token
tokenlex = choice $ map (\(c,t) -> const t <$> symbol c) table

-- recursive descendent parser

expr :: [Token] -> Either String Expr
expr ts
  = case runParser exprParser ts of
      ((t, []) : _) -> Right t
      _             -> Left "Parse error"

exprParser :: Parser Token Expr
exprParser = addtable `gen` termParser
  where
    addtable = [(Add, (:+:))]

termParser :: Parser Token Expr
termParser = multable `gen` factParser
  where
    multable = [(Mult, (:*:))]

factParser :: Parser Token Expr
factParser = numParser   <|>
             varParser  <|>
             parenExpr
  where
    parenExpr = pack lparen exprParser rparen
    lparen = symbol LParen
    rparen = symbol RParen

varParser :: Parser Token Expr
varParser = f <$> sat isVar
  where
    isVar (Id _) = True
    isVar _      = False
    f (Id v) = Var v
    f _ = error "impossible!"
 
numParser :: Parser Token Expr
numParser = f <$> sat isNum
  where
    isNum (Number _) = True
    isNum _ = False
    f (Number n) = Lit n
    f _          = error "impossible!"
