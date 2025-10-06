{-|
Module      : Parser.ParsedTree
Description : Generation of parsers for syntax trees (Parsed Trees).
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions to generate parsers based on PEG ('Grammar'),
producing syntax trees ('ParsedTree') as a result.
-}
module PEG.Pattern.Parser.ParsedTree (mkParser) where

import PEG.Pattern.Syntax.Base (Terminal(..), NonTerminal(..))
import PEG.Pattern.Syntax.Peg (Grammar, Expression(..), expression)
import PEG.Pattern.Syntax.ParsedTree (ParsedTree(..), flatten)
import PEG.Pattern.Parser.Base (Parser, blank)
import Text.Megaparsec (notFollowedBy, many, eof, optional, try, (<?>))
import Text.Megaparsec.Char (string)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))
import qualified Text.Megaparsec.Char.Lexer as Lexer


{-|
Generates a parser for a PEG.

The 'mkParser' function receives a PEG ('Grammar') and returns a parser that,
when applied to an input string, produces a corresponding AST ('ParsedTree').

The parser is based on the initial non-terminal of the grammar.

=== Usage examples:

>>> let grammar = ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S")
>>> parseWith (mkParser grammar) "ab"
Right (ParsedNT (NT "S") (ParsedSeq (ParsedT (T "a")) (ParsedT (T "b"))))

@since 1.0.0
-}
mkParser :: Grammar -> Parser ParsedTree
mkParser g@(_, nt) = ParsedNT nt <$> mkParser' g (fromJust $ PEG.Pattern.Syntax.Peg.expression g nt)
                        <* optional blank
                        <* eof

{-|
Parser for a terminal.

Receives a terminal ('Terminal') and returns a parser that consumes the string corresponding
to the input terminal.

@since 1.0.0
-}
terminal :: Terminal -> Parser Terminal
terminal (T t) = T <$> string t

{-|
Generates a parser for a PEG expression.

The 'mkParser'' function is used internally by 'mkParser' to process different types
of PEG expressions ('Expression') and produce the corresponding syntax tree.

@since 1.0.0
-}
mkParser' :: Grammar -> Expression -> Parser ParsedTree
mkParser' _ Empty = ParsedEpsilon <$ string ""
mkParser' _ (ExprT t) = ParsedT <$> terminal t
mkParser' g (ExprNT nt@(NT n)) = ParsedNT nt <$> mkParser' g (fromJust $ PEG.Pattern.Syntax.Peg.expression g nt) <?> n
mkParser' g (Choice e1 e2) = try (ParsedChoiceLeft <$> mkParser' g e1)
                             <|> ParsedChoiceRight <$> mkParser' g e2
mkParser' g (Sequence e1 e2) = ParsedSeq <$> mkParser' g e1 <*> mkParser' g e2
mkParser' g (Star e) = ParsedStar <$> many (try $ mkParser' g e)
mkParser' g (Not e) = ParsedNot <$ notFollowedBy (mkParser' g e)
mkParser' g (Flatten e) = ParsedT . T . flatten <$> mkParser' g e
mkParser' g (Indent e b) = Lexer.indentBlock blank p
    where
        p = do
            expr <- mkParser' g e
            return $ Lexer.IndentSome Nothing (return . ParsedIndent expr) (try $ mkParser' g b)
