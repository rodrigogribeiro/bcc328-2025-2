{-|
Module      : Syntax.Pattern
Description : Definitions of patterns and utilities for grammars.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module defines structures to represent patterns over grammars,
as well as utility functions for manipulating and replacing named patterns.
It also provides instances of the 'Pretty' class for formatted printing.
-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, DeriveDataTypeable #-}
module PEG.Pattern.Syntax.Pattern
    ( Pattern(..)
    , SyntaxPattern(..)
    , NamedPattern
    , NamedSynPat
    , references
    , replaceSynPats
    ) where

import PEG.Pattern.Syntax.Base (NonTerminal(..), Terminal(..), Pretty(..))
import Text.PrettyPrint.HughesPJ ((<+>), text, parens, Doc, brackets)
import Data.List (nub)
import Data.Bifunctor (Bifunctor(second))
import Data.Generics (everything, mkQ, Typeable, Data, everywhere, mkT)
import PEG.Pattern.Syntax.Peg (Expression)

{-|
Represents a pattern in a grammar.

A 'Pattern' can be:
- 'PatEpsilon': Represents the empty pattern (ε).
- 'PatT': A terminal symbol.
- 'PatNT': A non-terminal symbol associated with another pattern.
- 'PatSeq': A sequence of two patterns.
- 'PatChoice': A choice between two patterns.
- 'PatStar': A repetition of zero or more times of a pattern.
- 'PatNot': A negation of a pattern.
- 'PatVar': A pattern associated with a variable.

@since 1.0.0
-}
data Pattern
    = PatEpsilon
    | PatT Terminal
    | PatNT NonTerminal Pattern
    | PatSeq Pattern Pattern
    | PatChoice Pattern Pattern
    | PatStar Pattern
    | PatStarSeq [Pattern]
    | PatNot Pattern
    | PatVar Expression String
    deriving (Eq, Show, Ord, Typeable, Data)

{-|
Represents a syntactic pattern in a grammar.

A 'SyntaxPattern' can be:
- 'SynEpsilon': Represents the empty pattern (ε).
- 'SynT': A terminal symbol.
- 'SynNT': A non-terminal symbol associated with another syntactic pattern.
- 'SynSeq': A sequence of two syntactic patterns.
- 'SynChoice': A choice between two syntactic patterns.
- 'SynStar': A repetition of zero or more times of a syntactic pattern.
- 'SynNot': A negation of a syntactic pattern.
- 'SynVar': A syntactic pattern associated with a variable.
- 'SynRef': A reference to a named pattern.

@since 1.0.0
-}
data SyntaxPattern
    = SynEpsilon
    | SynT Terminal
    | SynNT NonTerminal SyntaxPattern
    | SynSeq SyntaxPattern SyntaxPattern
    | SynChoice SyntaxPattern SyntaxPattern
    | SynStar SyntaxPattern
    | SynNot SyntaxPattern
    | SynVar Expression String
    | SynRef String
    deriving (Eq, Show, Ord, Typeable, Data)

{-|
A named pattern, which associates a name ('String') with a 'Pattern'.

@since 1.0.0
-}
type NamedPattern = (String, Pattern)

{-|
A named syntactic pattern, which associates a name ('String') with a 'SyntaxPattern'.

@since 1.0.0
-}
type NamedSynPat = (String, SyntaxPattern)

{-|
Instance of the 'Pretty' class for 'Pattern'.

Prints the pattern in a readable format, with operators like `/` for choice,
`*` for repetition, and `!` for negation.

@since 1.0.0
-}
instance Pretty Pattern where
    pPrint :: Pattern -> Doc
    pPrint (PatNT nt p)      = pPrint nt <+> text ":=" <+> parens (pPrint p)
    pPrint (PatT t)          = pPrint t
    pPrint (PatVar s name)   = text ("#" ++ name) <> text ":" <> parens (pPrint s)
    pPrint PatEpsilon        = text "ε"
    pPrint (PatSeq p1 p2)    = pPrint p1 <+> pPrint p2
    pPrint (PatChoice p1 p2) = parens $ pPrint p1 <+> text "/" <+> pPrint p2
    pPrint (PatStar p)       = parens (pPrint p) <> text "*"
    pPrint (PatStarSeq ps)   = (brackets . pPrint) ps
    pPrint (PatNot p)        = text "!" <> parens (pPrint p)

{-|
Instance of the 'Pretty' class for 'SyntaxPattern'.

Prints the syntactic pattern in a readable format, with operators like `/` for choice,
`*` for repetition, and `!` for negation.

@since 1.0.0
-}
instance Pretty SyntaxPattern where
    pPrint :: SyntaxPattern -> Doc
    pPrint (SynNT nt ps)     = pPrint nt <+> text ":=" <+> parens (pPrint ps)
    pPrint (SynT t)          = pPrint t
    pPrint (SynVar s name)   = text ("#" ++ name) <> text ":" <> parens (pPrint s)
    pPrint SynEpsilon        = text "ε"
    pPrint (SynSeq p1 p2)    = pPrint p1 <+> pPrint p2
    pPrint (SynChoice p1 p2) = parens $ pPrint p1 <+> text "/" <+> pPrint p2
    pPrint (SynStar p)       = parens (pPrint p) <> text "*"
    pPrint (SynNot p)        = text "!" <> parens (pPrint p)
    pPrint (SynRef name)     = text $ "@" ++ name

{-|
Instance of the 'Pretty' class for 'NamedPattern'.

Prints the named pattern in the format `pattern <name> : <pattern>`.

@since 1.0.0
-}
instance Pretty NamedPattern where
    pPrint :: NamedPattern -> Doc
    pPrint (name, pat) = text ("pattern " ++ name ++ " :") <+> pPrint pat <+> text "\n"

{-|
Instance of the 'Pretty' class for 'NamedSynPat'.

Prints the named syntactic pattern in the format `pattern <name> : <pattern>`.

@since 1.0.0
-}
instance Pretty NamedSynPat where
    pPrint :: NamedSynPat -> Doc
    pPrint (name, syn) = text ("pattern " ++ name ++ " :") <+> pPrint syn <+> text "\n"

-------------------------------------------------------------------------------

{-|
Replaces all references in a 'SyntaxPattern' with their corresponding patterns,
based on a named pattern.

@since 1.0.0
-}
replaceInPat :: NamedSynPat -> SyntaxPattern -> SyntaxPattern
replaceInPat ref = everywhere $ mkT (replace ref)
    where
        replace (n, p) (SynRef n') = if n == n' then p else SynRef n'
        replace _ p                = p

{-|
Returns a list of pattern names referenced in a 'SyntaxPattern'.

=== Usage examples:

>>> references (SynSeq (SynRef "A") (SynRef "B"))
["A","B"]

@since 1.0.0
-}
references :: SyntaxPattern -> [String]
references = nub <$> everything (++) ([] `mkQ` refs)
    where
        refs (SynRef s) = [s]
        refs _ = []

{-|
Replaces all references in a list of patterns ordered by dependency.

@since 1.0.0
-}
replaceSynPats :: [NamedSynPat] -> [NamedSynPat]
replaceSynPats ps = foldr replaceSynPat ps ps

{-|
Replaces all references in a pattern within a list of patterns.

@since 1.0.0
-}
replaceSynPat :: NamedSynPat -> [NamedSynPat] -> [NamedSynPat]
replaceSynPat p = map (second $ replaceInPat p)
