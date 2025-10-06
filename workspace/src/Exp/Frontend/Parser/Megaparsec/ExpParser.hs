module Exp.Frontend.Parser.Megaparsec.ExpParser (expParser) where

import Exp.Frontend.Syntax.ExpSyntax
import Exp.Frontend.Lexer.Megaparsec.ExpLexer

import Text.Megaparsec
import Control.Monad.Combinators.Expr

-- definition of operator table

opTable :: [[Operator Parser Exp]]
opTable
  = [ [ infixL (:*:) "*" ]
    , [ infixL (:+:) "+" ]
    ]
 where
  infixL op sym = InfixL $ op <$ symbol sym

termP :: Parser Exp
termP = parens expP
    <|> EInt <$> int

expP :: Parser Exp
expP = makeExprParser termP opTable

expParser :: String -> Either String Exp
expParser s = case parse expP "" s of
                Left err -> Left (errorBundlePretty err)
                Right e  -> Right e
