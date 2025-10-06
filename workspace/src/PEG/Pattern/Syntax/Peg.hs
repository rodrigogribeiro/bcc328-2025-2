{-|
Module      : Syntax.Peg
Description : Definitions for PEGs (Parsing Expression Grammars).
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module defines structures and functions to work with PEGs (Parsing Expression Grammars),
including expressions, definitions, and complete grammars. It also provides instances of the 'Pretty'
class for formatted printing.
-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, DeriveDataTypeable #-}
module PEG.Pattern.Syntax.Peg
    ( Expression(..)
    , Definition
    , Grammar
    , ExpressionZipper
    , nonTerminals
    , terminals
    , terminals'
    , expression
    , produces
    , goLeft
    , goRight
    , goUp
    , goDown
    , pullFromRight
    ) where

import PEG.Pattern.Syntax.Base (NonTerminal, Terminal, Pretty(..))
import Text.PrettyPrint.HughesPJ (text, Doc, maybeParens, (<+>), (<>), parens)
import Prelude hiding ((<>))
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Generics (Data, Typeable, mkQ, everything)
import Data.Foldable (find)

{-|
Represents an expression in a PEG.

An expression can be:
- 'Empty': Represents the empty expression (ε).
- 'ExprT': A terminal symbol.
- 'ExprNT': A non-terminal symbol.
- 'Sequence': A sequence of two expressions.
- 'Choice': An ordered choice between two expressions.
- 'Star': A repetition of zero or more times of an expression.
- 'Not': A negation of an expression.
- 'Flatten': An expression that must be "flattened".
- 'Indent': An expression that must be "indented" with respect to the first. Because of the way MegaParsec parses indented blocks, the second expressions turns into a e+.

@since 1.0.0
-}
data Expression
    = Empty
    | ExprT Terminal
    | ExprNT NonTerminal
    | Sequence Expression Expression
    | Choice Expression Expression
    | Star Expression
    | Not Expression
    | Flatten Expression
    | Indent Expression Expression
    deriving (Show, Eq, Ord, Typeable, Data)

-- | Represents the context ("crumbs") for navigating the expression zipper.
-- Each constructor indicates the origin of the current node in the expression tree.
data ExpressionCrumb
    = SequenceFirst Expression
    | SequenceSecond Expression
    | ChoiceFirst Expression
    | ChoiceSecond Expression
    | IndentFirst Expression
    | IndentSecond Expression
    | StarCrumb
    | NotCrumb
    | FlattenCrumb

type ExpressionPath = [ExpressionCrumb]

-- | Zipper for efficient navigation in the expression tree.
type ExpressionZipper = (Expression, ExpressionPath)

{-|
Moves up one level in the expression tree using the zipper.

Returns 'Nothing' if it is not possible to go up.
-}
goUp :: ExpressionZipper -> Maybe ExpressionZipper
goUp (e1, SequenceFirst e2:z)  = Just (Sequence e1 e2, z)
goUp (e2, SequenceSecond e1:z) = Just (Sequence e1 e2, z)
goUp (e1, IndentFirst e2:z)    = Just (Indent e1 e2, z)
goUp (e2, IndentSecond e1:z)   = Just (Indent e1 e2, z)
goUp (e1, ChoiceFirst e2:z)    = Just (Choice e1 e2, z)
goUp (e2, ChoiceSecond e1:z)   = Just (Choice e1 e2, z)
goUp (e1, StarCrumb:z)         = Just (Star e1, z)
goUp (e1, NotCrumb:z)          = Just (Not e1, z)
goUp (e1, FlattenCrumb:z)      = Just (Flatten e1, z)
goUp (_, [])                   = Nothing

{-|
Moves to the right element in the expression tree using the zipper.

Returns 'Nothing' if there is no right element.
-}
goRight :: ExpressionZipper -> Maybe ExpressionZipper
goRight (Sequence e1 e2, z) = Just (e2, SequenceSecond e1:z)
goRight (Choice e1 e2, z)   = Just (e2, ChoiceSecond e1:z)
goRight (Indent e1 e2, z)   = Just (e2, IndentSecond e1:z)
goRight _                   = Nothing

{-|
Moves to the left element in the expression tree using the zipper.

Returns 'Nothing' if there is no left element.
-}
goLeft :: ExpressionZipper -> Maybe ExpressionZipper
goLeft (Sequence e1 e2, z) = Just (e1, SequenceFirst e2:z)
goLeft (Choice e1 e2, z)   = Just (e1, ChoiceFirst e2:z)
goLeft (Indent e1 e2, z)   = Just (e1, IndentFirst e2:z)
goLeft _                   = Nothing

{-|
Moves down to the next level in the expression tree using the zipper.

Returns 'Nothing' if it is not possible to go down.
-}
goDown :: ExpressionZipper -> Maybe ExpressionZipper
goDown (Star e, z)    = Just (e, StarCrumb:z)
goDown (Not e, z)     = Just (e, NotCrumb:z)
goDown (Flatten e, z) = Just (e, FlattenCrumb:z)
goDown _              = Nothing

{-|
Extracts the rightmost element from a sequence, if possible.

Returns 'Nothing' if it is not a sequence.
-}
pullFromRight :: Expression -> Maybe Expression
pullFromRight (Sequence e1 e2) = maybe (Just e1') (Just . Sequence e1') eT
    where
        (eH, eT) = getHead e2
        e1' = addAtEnd e1 eH
pullFromRight _ = Nothing

-- | Adds an expression to the end of a sequence.
addAtEnd :: Expression -> Expression -> Expression
addAtEnd (Sequence e1 e2) e3 = Sequence e1 $ addAtEnd e2 e3
addAtEnd e e3 = Sequence e e3

-- | Gets the first element of a sequence and the remainder, if any.
getHead :: Expression -> (Expression, Maybe Expression)
getHead (Sequence e1 e2) = (e1, Just e2)
getHead e                = (e, Nothing)

{-|
Represents a definition in a PEG.

A definition associates a 'NonTerminal' with an 'Expression'.

@since 1.0.0
-}
type Definition = (NonTerminal, Expression)

{-|
Represents a PEG.

A grammar consists of a list of 'Definition' and an initial 'NonTerminal'.

@since 1.0.0
-}
type Grammar = ([Definition], NonTerminal)

-- Auxiliary functions to determine when to use parentheses
parensSeq :: Expression -> Bool
parensSeq (Choice _ _) = True
parensSeq _ = False

parensNot :: Expression -> Bool
parensNot (Choice _ _) = True
parensNot (Sequence _ _) = True
parensNot _ = False

parensStar :: Expression -> Bool
parensStar (Choice _ _) = True
parensStar (Sequence _ _) = True
parensStar (Not _) = True
parensStar _ = False

{-|
Instance of the 'Pretty' class for 'Expression'.

Prints the expression in a readable format, with operators like `/` for choice,
`*` for repetition, and `!` for negation.

@since 1.0.0
-}
instance Pretty Expression where
    pPrint :: Expression -> Doc
    pPrint Empty            = text "ε"
    pPrint (ExprT t)        = pPrint t
    pPrint (ExprNT nt)      = pPrint nt
    pPrint (Sequence e1 e2) = maybeParens (parensSeq e1) (pPrint e1)
                                <+> maybeParens (parensSeq e2) (pPrint e2)
    pPrint (Choice e1 e2)   = pPrint e1 <+> text "/" <+> pPrint e2
    pPrint (Star e)         = maybeParens (parensStar e) (pPrint e) <> text "*"
    pPrint (Not e)          = text "!" <> maybeParens (parensNot e) (pPrint e)
    pPrint (Flatten e)      = text "^" <> parens (pPrint e)
    pPrint (Indent e b)      = parens (pPrint e) <+> text ">" <+> parens (pPrint b)

{-|
Instance of the 'Pretty' class for 'Definition'.

Prints a definition in the format `<non-terminal> <- <expression>`.

@since 1.0.0
-}
instance Pretty Definition where
    pPrint :: Definition -> Doc
    pPrint (nt, e) = pPrint nt <+> text "<-" <+> pPrint e <> text "\n"

{-|
Instance of the 'Pretty' class for 'Grammar'.

Prints all definitions of a grammar.

@since 1.0.0
-}
instance Pretty Grammar where
    pPrint :: Grammar -> Doc
    pPrint (ds, _) = pPrint ds

-------------------------------------------------------------------------------

{-|
Returns the list of non-terminals of a grammar.

=== Usage examples:

>>> nonTerminals ([(NT "S", Empty), (NT "A", ExprT (T "a"))], NT "S")
[NT "S",NT "A"]

@since 1.0.0
-}
nonTerminals :: Grammar -> [NonTerminal]
nonTerminals (ds, _) = map fst ds

{-|
Returns the list of terminals present in an expression.

=== Usage examples:

>>> terminals' (Sequence (ExprT (T "a")) (ExprT (T "b")))
[T "a",T "b"]

@since 1.0.0
-}
terminals' :: Expression -> [Terminal]
terminals' = nub <$> everything (++) ([] `mkQ` terminal)
    where
        terminal (ExprT t) = [t]
        terminal _ = []

{-|
Returns the list of terminals present in a grammar.

=== Usage examples:

>>> terminals ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S")
[T "a",T "b"]

@since 1.0.0
-}
terminals :: Grammar -> [Terminal]
terminals (ds, _) = nub $ concatMap (terminals' . snd) ds

{-|
Returns the expression associated with a non-terminal in a grammar, if it exists.

=== Usage examples:

>>> expression ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S") (NT "S")
Just (Sequence (ExprT (T "a")) (ExprT (T "b")))

>>> expression ([(NT "S", Empty)], NT "S") (NT "A")
Nothing

@since 1.0.0
-}
expression :: Grammar -> NonTerminal -> Maybe Expression
expression (rs, _) nt = snd <$> find (\ x -> fst x == nt) rs

{-|
Checks if a non-terminal produces a terminal in a grammar.

=== Usage examples:

>>> produces ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S") (NT "S") (T "a")
True

>>> produces ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S") (NT "S") (T "b")
True

>>> produces ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S") (NT "S") (T "c")
False

@since 1.0.0
-}
produces :: Grammar -> NonTerminal -> Terminal -> Bool
produces g nt t = isJust produce
    where
        expr = expression g nt
        terms = terminals' <$> expr
        produce = find (== t) =<< terms
