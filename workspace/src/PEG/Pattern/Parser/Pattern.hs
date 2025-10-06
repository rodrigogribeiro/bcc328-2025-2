{-|
Module      : Parser.Pattern
Description : Parser for syntactic patterns in grammars.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides parsers for syntactic patterns ('SyntaxPattern') and named patterns
('NamedSynPat') in grammars. It also includes a function to execute the parser on an
input string.
-}
module PEG.Pattern.Parser.Pattern
    ( patterns
    , parsePatterns
    ) where

import PEG.Pattern.Syntax.Pattern (SyntaxPattern(..), NamedSynPat)
import PEG.Pattern.Parser.Base
    (Parser, sc, hsc, symbol, parens, identifier, nonTerminal, terminal, parseWith, ParseError)
import qualified PEG.Pattern.Parser.Peg as Peg
import Text.Megaparsec (eof, (<?>), choice, some, sepBy1, MonadParsec (try))
import Text.Megaparsec.Char (char)
import Control.Monad.Combinators.Expr (Operator(Postfix, Prefix), makeExprParser)

-------------------------------------------------------------------------------
--- SyntaxPattern Parser

{-|
Parser for a list of patterns.

Each pattern is preceded by the keyword `pattern` and associated with a name.

@since 1.0.0
-}
patterns :: Parser [NamedSynPat]
patterns = id <$ hsc <*> some pat <* eof

{-|
Parser for a pattern.

A pattern is defined in the format:

> pattern <name> : <pattern>

@since 1.0.0
-}
pat :: Parser NamedSynPat
pat =
    f <$> symbol "pattern"
        <*> identifier
        <*> symbol ":"
        <*> patNT
        <* sc
        <?> "pattern"
    where
        f _ name _ p = (name, p)

{-|
Parser for a primary expression.

A primary expression can be:
- An expression enclosed in parentheses.
- A non-terminal associated with a pattern.
- A terminal.
- A pattern variable.
- A reference to a named pattern.
- The empty symbol ('ε').

@since 1.0.0
-}
primary :: Parser SyntaxPattern
primary = choice
            [ try $ parens patNT
            , parens expression
            , patT
            , patVar
            , reference
            , epsilon
            ]

{-|
Parser for an expression composed of ordered choices.

Choices are separated by the `/` operator.

@since 1.0.0
-}
expression :: Parser SyntaxPattern
expression = foldr1 SynChoice <$> PEG.Pattern.Parser.Pattern.sequence `sepBy1` symbol "/"

{-|
Parser for a sequence of expressions.

Expressions are combined with the 'SynSeq' operator.

@since 1.0.0
-}
sequence :: Parser SyntaxPattern
sequence = foldr1 SynSeq <$> some prefix

{-|
Parser for a non-terminal associated with a pattern.

A non-terminal is followed by the `:=` operator and an expression.

@since 1.0.0
-}
patNT :: Parser SyntaxPattern
patNT = f <$> nonTerminal <*> symbol ":=" <*> expression
    where f nt _ = SynNT nt

{-|
Parser for a terminal.

A terminal is represented by a literal.

@since 1.0.0
-}
patT :: Parser SyntaxPattern
patT = SynT <$> terminal

{-|
Parser for a pattern variable.

A pattern variable is defined in the format:

> #<name>:<expression>

@since 1.0.0
-}
patVar :: Parser SyntaxPattern
patVar = f <$> char '#' <*> identifier <*> char ':' <*> Peg.primary <?> "pattern variable"
    where
        f _ n _ s = SynVar s n

{-|
Parser for a reference to a named pattern.

A reference is preceded by the `@` character and followed by the pattern name.

@since 1.0.0
-}
reference :: Parser SyntaxPattern
reference = f <$> char '@' <*> identifier <?> "pattern name"
    where
        f _ = SynRef

{-|
Parser for the empty symbol ('ε').

@since 1.0.0
-}
epsilon :: Parser SyntaxPattern
epsilon = SynEpsilon <$ symbol "ε"

{-|
Parser for expressions with prefix and suffix operators.

The available operators are:
- `*`: Repetition zero or more times ('SynStar', 'PatStar').
- `+`: Repetition one or more times (`SynSeq e (SynStar e)`, `PatSeq e (PatStar e)`).
- `?`: Optional (`SynChoice e SynEpsilon`, `PatChoice e PatEpsilon`).
- `!`: Negation ('SynNot', 'PatNot').
- `&`: And (`SynNot . SynNot`, `PatNot . PatNot`).

@since 1.0.0
-}
prefix :: Parser SyntaxPattern
prefix = makeExprParser primary patOperatorTable

{-|
Operator table for syntactic patterns.

Defines the available prefix and suffix operators.

@since 1.0.0
-}
patOperatorTable :: [[Operator Parser SyntaxPattern]]
patOperatorTable =
    [
        [
            patSuffix "*" SynStar,
            patSuffix "+" plus,
            patSuffix "?" opt
        ],
        [
            patPrefix "!" SynNot,
            patPrefix "&" (SynNot . SynNot)
        ]
    ]
    where
        plus e = SynSeq e (SynStar e)
        opt e = SynChoice e SynEpsilon

{-|
Defines a suffix operator for patterns.

@since 1.0.0
-}
patSuffix :: String -> (SyntaxPattern -> SyntaxPattern) -> Operator Parser SyntaxPattern
patSuffix name f = Postfix (f <$ symbol name)

{-|
Defines a prefix operator for patterns.

@since 1.0.0
-}
patPrefix :: String -> (SyntaxPattern -> SyntaxPattern) -> Operator Parser SyntaxPattern
patPrefix name f = Prefix (f <$ symbol name)

{-|
Executes the pattern parser on an input string.

Returns an 'Either' containing the parser result or a parsing error.

=== Usage examples:

>>> parsePatterns "pattern A : S := \"a\" / \"b\""
Right [("A",SynNT (NT "S") (SynChoice (SynT (T "a")) (SynT (T "b"))))]

>>> parsePatterns "pattern B : S := #x:T"
Right [("B",SynNT (NT "S") (SynVar (Left (NT "T")) "x"))]

>>> parsePatterns "invalid"
Left ...

@since 1.0.0
-}
parsePatterns :: String -> Either ParseError [NamedSynPat]
parsePatterns = parseWith patterns
