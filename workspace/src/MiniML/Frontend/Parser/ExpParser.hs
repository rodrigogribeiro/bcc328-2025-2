{-# LANGUAGE OverloadedStrings #-}

module MiniML.Frontend.Parser.ExpParser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import MiniML.Frontend.Syntax.Exp
import MiniML.Frontend.Syntax.Type

type Parser = Parsec Void String

parser :: String -> Either String Exp
parser s
  = case runParser pExp "" s of
      Left err -> Left $ errorBundlePretty err
      Right e -> Right e

-- expression parser

pExp :: Parser Exp
pExp = pLet <|> pLam <|> pApp

pTerm :: Parser Exp
pTerm = Lit <$> pLit
      <|> Var <$> pIdentifier
      <|> parens pExp

pIdentifier :: Parser Name
pIdentifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check s = if elem s reserved then fail "reserved" else pure s

pLit :: Parser Lit
pLit =  LInt <$> lexeme L.decimal
    <|> LBool <$> (True <$ symbol "true" <|> False <$ symbol "false")

pApp :: Parser Exp
pApp = foldl1 App <$> some pTerm

pLam :: Parser Exp
pLam = do
    _ <- symbol "\\"
    var <- pIdentifier
    mTy <- pOptionalType
    _ <- symbol "."
    body <- pExp
    return $ Lam var mTy body

pLet :: Parser Exp
pLet = do
    _ <- symbol "let"
    (n,mt,v) <- pBinding
    _ <- symbol "in"
    body <- pExp
    return $ Let n mt v body
  where
    pBinding = do
        name <- pIdentifier
        mTy <- pOptionalScheme
        _ <- symbol "="
        val <- pExp
        return (name, mTy, val)


-- parser for types

pType :: Parser Type
pType = do
          t1 <- pTypeAtom
          (do
              _ <- symbol "->"
              t2 <- pType
              return (TFun t1 t2)) <|> return t1

pTypeAtom :: Parser Type
pTypeAtom = (TInt <$ symbol "int")
        <|> (TBool <$ symbol "bool")
        <|> (TVar <$> pTyVar)
        <|> parens pType

pTyVar :: Parser Name
pTyVar = pIdentifier

pOptionalType :: Parser (Maybe Type)
pOptionalType = optional (symbol ":" *> pType)

pOptionalScheme :: Parser (Maybe Scheme)
pOptionalScheme
  = optional $ symbol ":" *> do
          _ <- symbol "forall"
          vs <- some pIdentifier
          _ <- symbol "."
          t <- pType
          pure (Forall vs t)

-- Lexer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens p = f <$> symbol "(" <*> p <*> symbol ")"
  where
    f _ x _ = x

reserved :: [String]
reserved =
    [ "let"
    , "in"
    , "fn"
    , "true"
    , "false"
    , "int"
    , "bool"
    , "string"
    , "forall"
    ]


