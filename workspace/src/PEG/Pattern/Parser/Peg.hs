{-|
Module      : Parser.Peg
Description : Parser for PEGs (Parsing Expression Grammars).
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides parsers for PEGs (Parsing Expression Grammars),
including definitions, expressions, and PEG operators. It also provides a function
to execute the parser on an input string.
-}
module PEG.Pattern.Parser.Peg
    ( grammar
    , parseGrammar
    , expression
    , primary
    ) where

import PEG.Pattern.Syntax.Base (NonTerminal(NT), Terminal(..))
import PEG.Pattern.Syntax.Peg (Grammar, Definition, Expression(..))
import PEG.Pattern.Parser.Base
    (Parser, sc, symbol, parens, nonTerminal, terminal, hsc, parseWith, ParseError)
import Text.Megaparsec
    (eof, choice, some, sepBy1, someTill, try, optional)
import Text.Megaparsec.Char (alphaNumChar, char, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr (Operator(Postfix, Prefix, InfixL), makeExprParser)

-------------------------------------------------------------------------------
--- PEG parser

{-|
Parser for a complete PEG.

A grammar consists of a list of definitions and an initial non-terminal.
The first definition is considered the initial expression of the PEG.

@since 1.0.0
-}
grammar :: Parser Grammar
grammar = f <$> sc <*> some definition <* eof
    where
        f _ d = (d, fst $ head d)

{-|
Parser for a definition in a PEG.

A definition associates a non-terminal with an expression. If the definition is preceded
by a `^`, the expression will be flattened ('Flatten').

@since 1.0.0
-}
definition :: Parser Definition
definition = f <$> optional (string "^") <*> nonTerminal <*> symbol "<-" <*> expression <* sc
    where
        f flat nt _ e = case flat of
                        Nothing -> (nt, e)
                        Just _  -> (nt, Flatten e)

{-|
Parser for an expression in a PEG.

An expression can consist of choices ('Choice') separated by `/`.

@since 1.0.0
-}
expression :: Parser Expression
expression = foldr1 Choice <$> PEG.Pattern.Parser.Peg.sequence `sepBy1` symbol "/"

{-|
Parser for a sequence of expressions.

A sequence consists of multiple expressions combined with the 'Sequence' operator.

@since 1.0.0
-}
sequence :: Parser Expression
sequence = foldr1 Sequence <$> some prefix

{-|
Parser for a primary expression.

A primary expression can be:
- An empty symbol ('ε').
- A non-terminal.
- A terminal.
- An expression in parentheses.
- A character class.
- A dot (`.`), which represents any terminal in the PEG.

@since 1.0.0
-}
primary :: Parser Expression
primary = choice
            [ epsilon
            , ExprNT <$> nonTerminal
            , parens expression
            , ExprT <$> terminal
            , pClass
            , dot
            ]

{-|
Parser for a character class.

A character class is enclosed in brackets (`[ ]`) and can contain ranges
(e.g., `a-z`) or individual characters.

@since 1.0.0
-}
pClass :: Parser Expression
pClass = foldr1 Choice <$> (char '[' >> someTill range (char ']')) <* hsc

{-|
Parser for a range or individual character in a character class.

A range is defined as `a-z`, while an individual character is a single
literal character.

@since 1.0.0
-}
range :: Parser Expression
range = choice [
            try $ f <$> alphaNumChar <*> char '-' <*> alphaNumChar,
            expr <$> Lexer.charLiteral
        ]
        where
            f a _ b = foldr1 Choice $ map expr [a..b]
            expr = ExprT . T . (:[])

{-|
Parser for the dot (`.`), which represents any character.

@since 1.0.0
-}
dot :: Parser Expression
dot = ExprNT (NT ".") <$ symbol "."

{-|
Parser for the empty symbol ('ε').

@since 1.0.0
-}
epsilon :: Parser Expression
epsilon = Empty <$ symbol "ε"

{-|
Parser for expressions with prefix and suffix operators.

The available operators are:
- `*`: Zero or more repetitions ('Star').
- `+`: One or more repetitions (`Sequence e (Star e)`).
- `?`: Optional (`Choice e Empty`).
- `!`: Negation ('Not').
- `&`: And (`Not . Not`).

@since 1.0.0
-}
prefix :: Parser Expression
prefix = makeExprParser primary pegOperatorTable

{-|
PEG operator table.

Defines the available prefix and suffix operators.

@since 1.0.0
-}
pegOperatorTable :: [[Operator Parser Expression]]
pegOperatorTable =
    [ [ pegSuffix "*" Star
        , pegSuffix "+" plus
        , pegSuffix "?" opt
        ]
    , [ pegPrefix "!" Not
        , pegPrefix "&" (Not . Not)
        ]
    , [ pegInfix ">" Indent ]
    ]
    where
        plus e = Sequence e (Star e)
        opt e = Choice e Empty

{-|
Defines a suffix operator for PEG expressions.

@since 1.0.0
-}
pegSuffix :: String -> (Expression -> Expression) -> Operator Parser Expression
pegSuffix name f = Postfix (f <$ symbol name)

{-|
Defines a prefix operator for PEG expressions.

@since 1.0.0
-}
pegPrefix :: String -> (Expression -> Expression) -> Operator Parser Expression
pegPrefix name f = Prefix (f <$ symbol name)

{-|
Defines a infix operator for PEG expressions.

@since 1.0.0
-}
pegInfix :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
pegInfix name f = InfixL (f <$ symbol name)

{-|
Executes the PEG parser on an input string.

Returns an 'Either' containing the parser result or a parsing error.

=== Usage examples:

>>> parseGrammar "S <- \"a\" / \"b\""
Right ([(NT "S",Choice (ExprT (T "a")) (ExprT (T "b")))],NT "S")

>>> parseGrammar "S <- \"a\" \"b\""
Right ([(NT "S",Sequence (ExprT (T "a")) (ExprT (T "b")))],NT "S")

>>> parseGrammar "invalid"
Left ...

@since 1.0.0
-}
parseGrammar :: String -> Either ParseError Grammar
parseGrammar = parseWith grammar
