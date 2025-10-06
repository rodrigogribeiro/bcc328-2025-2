module Markup.Language.Parser (parseDocument) where

import Markup.Language.Syntax

import Data.Void
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


-- top level parsing function

parseDocument :: String -> Either String Document
parseDocument s
  = case parse pDocument "" s of
      Left err -> Left (errorBundlePretty err)
      Right d  -> Right d

-- type for parsers

type Parser = Parsec Void String

-- skipping white space and comments

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "//"
  blockCmnt = L.skipBlockComment "/*" "*/"


-- defining simple parsers to be used in lexical analysis

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = lexeme . L.symbol sc

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- basic parsers for markup documents

pDocument :: Parser Document
pDocument = (sc *> pLine) `sepBy` newline

pLine :: Parser Structure
pLine = choice [ pUnorderedList
               , pCodeBlock
               , pHeading
               , pParagraph
               ]

pHeading :: Parser Structure
pHeading = Heading <$> pLevel <*> pString <?> "heading"

pString :: Parser String
pString = many (satisfy (flip notElem "\n*-#>"))

pLevel :: Parser Natural
pLevel = f <$> many1 (symbol "*") <?> "level"
  where
    f = fromInteger . toInteger . length

pUnorderedList :: Parser Structure
pUnorderedList = (UnorderedList . wrap) <$> p
  where
    p = symbol "-" *> pString

pCodeBlock :: Parser Structure
pCodeBlock = (CodeBlock . wrap) <$> p
  where
    p = symbol ">" *> pString

pParagraph :: Parser Structure
pParagraph = Paragraph <$> pString

wrap :: a -> [a]
wrap x = [x]
