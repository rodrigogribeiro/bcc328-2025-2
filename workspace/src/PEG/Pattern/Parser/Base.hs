{-|
Module      : Parser.Base
Description : Basic definitions for grammar parsing.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides basic definitions for grammar parsing, including parsers
for non-terminals, terminals, and symbols, as well as utilities for handling
whitespace, comments, and delimiters.
-}
module PEG.Pattern.Parser.Base
    ( Parser
    , ParseError
    , nonTerminal
    , terminal
    , pSymbol
    , parseWith
    , blank
    , sc
    , hsc
    , symbol
    , symbolNL
    , parens
    , brackets
    , curly
    , identifier
    , litTerminal
    ) where

import PEG.Pattern.Syntax.Base (NonTerminal(..), Terminal(..), Symbol)
import Data.Void (Void)
import Text.Megaparsec
    ((<|>), empty, Parsec, between, many, (<?>), parse, someTill, ParseErrorBundle)
import Text.Megaparsec.Char (space1, hspace1, letterChar, alphaNumChar, char, eol)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

{-|
Type for parsers based on 'Text.Megaparsec'.

@since 1.0.0
-}
type Parser = Parsec Void String

{-|
Type for parsing errors.

@since 1.0.0
-}
type ParseError = ParseErrorBundle String Void

-------------------------------------------------------------------------------
--- Helpers

{-|
Parser that consumes whitespace.

@since 1.0.0
-}
blank :: Parser ()
blank = Lexer.space space1 empty empty

{-|
Parser that consumes whitespace and line comments.

Line comments start with `--`.

@since 1.0.0
-}
sc :: Parser ()
sc = Lexer.space space1 lineComment empty

{-|
Parser that consumes horizontal spaces and line comments.

Line comments start with `--`.

@since 1.0.0
-}
hsc :: Parser ()
hsc = Lexer.space hspace1 lineComment empty

{-|
Parser for line comments.

Line comments start with `--` and end at the end of the line.

@since 1.0.0
-}
lineComment :: Parser ()
lineComment = void (Lexer.skipLineComment "--") <* eol

{-|
Parser that applies a parser and consumes horizontal spaces after it.

@since 1.0.0
-}
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme hsc

{-|
Parser for symbols delimited by horizontal spaces.

@since 1.0.0
-}
symbol :: String -> Parser String
symbol = Lexer.symbol hsc

{-|
Parser for symbols delimited by spaces or newlines.

@since 1.0.0
-}
symbolNL :: String -> Parser String
symbolNL = Lexer.symbol sc

{-|
Parser for expressions delimited by parentheses.

@since 1.0.0
-}
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

{-|
Parser for expressions delimited by brackets.

@since 1.0.0
-}
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

{-|
Parser for expressions delimited by curly braces.

@since 1.0.0
-}
curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

{-|
Parser for identifiers.

An identifier starts with a letter and can contain letters, numbers, or `_`.

@since 1.0.0
-}
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

{-|
Parser for literals (terminals).

A literal is a non-empty string delimited by double quotes (`"`) or single quotes (`'`).

@since 1.0.0
-}
litTerminal :: Parser String
litTerminal = lexeme (char '"' >> someTill Lexer.charLiteral (char '"') <?> "string")
            -- <|> lexeme (char '\'' >> someTill Lexer.charLiteral (char '\'') <?> "string")

-------------------------------------------------------------------------------
--- Base parser

{-|
Parser for non-terminals.

A non-terminal is represented by an identifier.

@since 1.0.0
-}
nonTerminal :: Parser NonTerminal
nonTerminal =
    NT <$> identifier <?> "nonTerminal"

{-|
Parser for terminals.

A terminal is represented by a literal.

@since 1.0.0
-}
terminal :: Parser Terminal
terminal = T <$> litTerminal <?> "terminal"

{-|
Parser for symbols.

A symbol can be a 'NonTerminal' or a 'Terminal'.

@since 1.0.0
-}
pSymbol :: Parser Symbol
pSymbol = Left <$> nonTerminal <|> Right <$> terminal

{-|
Function to execute a parser on an input string.

Returns an 'Either' containing the parser result or a parsing error.

=== Usage examples:

>>> parseWith nonTerminal "S"
Right (NT "S")

>>> parseWith terminal "\"a\""
Right (T "a")

>>> parseWith terminal "invalid"
Left ...

@since 1.0.0
-}
parseWith :: Parser a -> String -> Either ParseError a
parseWith p = parse p ""
