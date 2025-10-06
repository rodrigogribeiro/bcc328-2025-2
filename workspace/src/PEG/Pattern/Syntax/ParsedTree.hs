{-|
Module      : Syntax.ParsedTree
Description : Representation of parsed trees.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the structure of a parsed tree ('ParsedTree') and
associated functions, such as the 'flatten' function to extract the terminals from a tree.
It also provides an instance of the 'Pretty' class for formatted printing.
-}
{-# LANGUAGE DeriveDataTypeable, InstanceSigs #-}
module PEG.Pattern.Syntax.ParsedTree
    ( ParsedTree(..)
    , ParsedTreeZipper
    , flatten
    , goUp
    , goDown
    , goLeft
    , goRight
    , pullFromRight
    , ofExpression
    ) where

import PEG.Pattern.Syntax.Base (Terminal(..), NonTerminal, Pretty(..))
import Text.PrettyPrint.HughesPJ (text, Doc, (<+>), (<>), empty, lbrack, rbrack, hcat)
import Prelude hiding ((<>))
import Data.Generics (Data, Typeable, mkQ, everything)
import PEG.Pattern.Syntax.Peg (Expression(..), Grammar, expression)

{-|
Represents an abstract syntax tree (AST).

A 'ParsedTree' can be:
- 'ParsedEpsilon': Represents the empty tree (ε).
- 'ParsedT': A terminal symbol.
- 'ParsedNT': A non-terminal symbol associated with a subtree.
- 'ParsedSeq': A sequence of two trees.
- 'ParsedChoiceLeft': Represents the left choice in a choice operation.
- 'ParsedChoiceRight': Represents the right choice in a choice operation.
- 'ParsedStar': Represents a repetition of zero or more times of a tree.
- 'ParsedNot': Represents the negation of a tree.

@since 1.0.0
-}
data ParsedTree
    = ParsedEpsilon
    | ParsedT Terminal
    | ParsedNT NonTerminal ParsedTree
    | ParsedSeq ParsedTree ParsedTree
    | ParsedChoiceLeft ParsedTree
    | ParsedChoiceRight ParsedTree
    | ParsedStar [ParsedTree]
    | ParsedNot
    | ParsedIndent ParsedTree [ParsedTree]
    deriving (Show, Typeable, Data)

-- | Represents the context ("crumbs") for navigating the parsed tree zipper.
-- Each constructor indicates the origin of the current node in the tree.
data ParsedTreeCrumbs
    = ParsedNTCrumb NonTerminal
    | ParsedSeqFirst ParsedTree
    | ParsedSeqSecond ParsedTree
    | ParsedChoiceLeftCrumb
    | ParsedChoiceRightCrumb
    | ParsedStarCrumb [ParsedTree] [ParsedTree] -- primeiro é o que falta, segundo é o que já foi
    | ParsedIndentFirst [ParsedTree]
    | ParsedIndentSecond ParsedTree

type ParsedTreePath = [ParsedTreeCrumbs]

-- | Zipper for efficient navigation in the parsed tree.
type ParsedTreeZipper = (ParsedTree, ParsedTreePath)

{-|
Moves up one level in the parsed tree using the zipper.

Returns 'Nothing' if it is not possible to go up.
-}
goUp :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goUp (t, ParsedNTCrumb nt:z)                 = Just (ParsedNT nt t, z)
goUp (t1, ParsedSeqFirst t2:z)               = Just (ParsedSeq t1 t2, z)
goUp (t2, ParsedSeqSecond t1:z)              = Just (ParsedSeq t1 t2, z)
goUp (t, ParsedChoiceLeftCrumb:z)            = Just (ParsedChoiceLeft t, z)
goUp (t, ParsedChoiceRightCrumb:z)           = Just (ParsedChoiceRight t, z)
goUp (t, (ParsedStarCrumb ts []):z)          = Just (ParsedStar (t:ts), z)
goUp z@(_, (ParsedStarCrumb _ _):_)          = goUp =<< goLeft z
goUp (t, ParsedIndentFirst ts:z)             = Just (ParsedIndent t ts, z)
goUp (ParsedStar ts, ParsedIndentSecond t:z) = Just (ParsedIndent t ts, z)
goUp (_, ParsedIndentSecond _:_)             = Nothing
goUp (_, [])                                 = Nothing

{-|
Moves down to the next level in the parsed tree using the zipper.

Returns 'Nothing' if it is not possible to go down.
-}
goDown :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goDown (ParsedNT nt t, z)       = Just (t, ParsedNTCrumb nt:z)
goDown (ParsedChoiceLeft t, z)  = Just (t, ParsedChoiceLeftCrumb:z)
goDown (ParsedChoiceRight t, z) = Just (t, ParsedChoiceRightCrumb:z)
goDown (ParsedStar [], _)       = Nothing
goDown (ParsedStar (t:ts), z)   = Just (t, ParsedStarCrumb ts []:z)
goDown _                        = Nothing

{-|
Moves to the left element in the parsed tree using the zipper.

Returns 'Nothing' if there is no left element.
-}
goLeft :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goLeft (_, (ParsedStarCrumb _ []):_)             = Nothing
goLeft (t, (ParsedStarCrumb ts (t':ts2)):z)      = Just (t', ParsedStarCrumb (t:ts) ts2:z)
goLeft (ParsedSeq t1 t2, z)                      = Just (t1, ParsedSeqFirst t2:z)
goLeft (ParsedIndent t ts, z)                    = Just (t, ParsedIndentFirst ts:z)
goLeft _                                         = Nothing

{-|
Moves to the right element in the parsed tree using the zipper.

Returns 'Nothing' if there is no right element.
-}
goRight :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goRight (_, (ParsedStarCrumb [] _):_)             = Nothing
goRight (t, (ParsedStarCrumb (t':ts1) ts):z)      = Just (t', ParsedStarCrumb ts1 (t:ts):z)
goRight (ParsedSeq t1 t2, z)                      = Just (t2, ParsedSeqSecond t1:z)
goRight (ParsedIndent t ts, z)                    = Just (ParsedStar ts, ParsedIndentSecond t:z)
goRight _                                         = Nothing

{-|
Extracts the rightmost element from a sequence, if possible.

Returns 'Nothing' if it is not a sequence.
-}
pullFromRight :: ParsedTree -> Maybe ParsedTree
pullFromRight (ParsedSeq e1 e2) = maybe (Just e1') (Just . ParsedSeq e1') eT
    where
        (eH, eT) = getHead e2
        e1' = addAtEnd e1 eH
pullFromRight _ = Nothing

-- | Adds a node to the end of a sequence.
addAtEnd :: ParsedTree -> ParsedTree -> ParsedTree
addAtEnd (ParsedSeq e1 e2) e3 = ParsedSeq e1 $ addAtEnd e2 e3
addAtEnd e e3 = ParsedSeq e e3

-- | Gets the first element of a sequence and the remainder, if any.
getHead :: ParsedTree -> (ParsedTree, Maybe ParsedTree)
getHead (ParsedSeq e1 e2) = (e1, Just e2)
getHead e                = (e, Nothing)

{-|
Instance of the 'Pretty' class for 'ParsedTree'.

Prints the syntax tree in a readable format, with indentation
and visual symbols to represent the tree hierarchy.

@since 1.0.0
-}
instance Pretty ParsedTree where
    pPrint :: ParsedTree -> Doc
    pPrint pt = pPrint' Text.PrettyPrint.HughesPJ.empty pt <> text "\n"

-- Auxiliary functions for tree formatting
nest :: Doc -> Doc
nest i = i <> text "├╴"

nest1 :: Doc -> Doc
nest1 i = i <> text "╰╴"

continue :: Doc -> Doc
continue i = i <> text "| "

continue1 :: Doc -> Doc
continue1 i = i <> text "  "

{-|
Auxiliary function for formatted printing of a 'ParsedTree'.

@since 1.0.0
-}
pPrint' :: Doc -> ParsedTree -> Doc
pPrint' _ ParsedEpsilon = text "ε"
pPrint' _ (ParsedT t) = pPrint t
pPrint' indent (ParsedNT nt tree) =
    text "NT" <+> pPrint nt <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedSeq t1 t2) =
    text "Seq" <> text "\n"
    <> nest indent <> pPrint' (continue indent) t1 <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) t2
pPrint' indent (ParsedChoiceLeft tree) =
    text "Left" <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedChoiceRight tree) =
    text "Right" <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedStar ts) =
    text "Star" <+> lbrack <> list' <> rbrack
    where
        listnest = if null ts then Text.PrettyPrint.HughesPJ.empty else text "\n"
        listEnd   = if null ts then Text.PrettyPrint.HughesPJ.empty else nest1 indent
        list      = hcat (map (\ x -> nest indent <> pPrint' (continue indent) x <> text "\n") ts)
        list'     = listnest <> list <> listEnd
pPrint' _ ParsedNot = Text.PrettyPrint.HughesPJ.empty
pPrint' indent (ParsedIndent e b) =
    text "Indent" <> text "\n"
    <> nest indent <> pPrint' (continue indent) e <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) (ParsedStar b)

{-|
Extracts all terminal symbols from a 'ParsedTree' as a single string.

=== Usage examples:

>>> flatten (ParsedSeq (ParsedT (T "a")) (ParsedT (T "b")))
"ab"

>>> flatten ParsedEpsilon
""

@since 1.0.0
-}
flatten :: ParsedTree -> String
flatten = everything (++) ("" `mkQ` term)
    where
        term (ParsedT (T t)) = t
        term _ = ""

{-|
Checks if a parsed tree matches an expression from a grammar.

Returns 'True' if it matches, 'False' otherwise.
-}
ofExpression :: Grammar -> Expression -> ParsedTree -> Bool
ofExpression _ Empty ParsedEpsilon = True
ofExpression _ (ExprT t) (ParsedT t') = t == t'
ofExpression g (ExprNT nt) (ParsedNT nt' t) = nt == nt' && case expression g nt of
                                                Just x -> ofExpression g x t
                                                Nothing -> False
ofExpression g (Sequence e1 e2) (ParsedSeq t1 t2) = ofExpression g e1 t1 && ofExpression g e2 t2
ofExpression g (Choice e1 _) (ParsedChoiceLeft t) = ofExpression g e1 t
ofExpression g (Choice _ e2) (ParsedChoiceRight t)  = ofExpression g e2 t
ofExpression g (Star e) (ParsedStar ts) = all (ofExpression g e) ts
ofExpression _ (Not _) ParsedNot = True
ofExpression _ (Flatten _) (ParsedT _) = True 
ofExpression g (Indent e1 e2) (ParsedIndent t ts) = ofExpression g e1 t && all (ofExpression g e2) ts
ofExpression _ _ _ = False
