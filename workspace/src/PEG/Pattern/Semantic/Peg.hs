{-|
Module      : Semantic.Peg
Description : Semantic analysis of PEGs (Parsing Expression Grammars).
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions for semantic analysis of PEGs ('Grammar'),
including rule validation, left recursion detection, nullable expressions,
and duplicate definitions. It also defines specific exceptions for semantic
errors in PEGs.
-}
module PEG.Pattern.Semantic.Peg
    ( PegException(..)
    , processPeg
    ) where

import PEG.Pattern.Syntax.Base (NonTerminal(..), duplicatesOfFirst, filterByFirst, Pretty(..))
import PEG.Pattern.Syntax.Peg (Expression(..), Definition, Grammar, terminals, expression)
import Data.List (nub)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Foldable (Foldable (toList))
import Data.Bifunctor (Bifunctor(first, second))
import Control.Exception (Exception)
import Data.Either (lefts, rights)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import Data.Generics (mkT, everywhere)
import Text.PrettyPrint.HughesPJ (Doc, text, (<+>), parens)

-------------------------------------------------------------------------------
--- Exceptions

{-|
Exception raised when a definition references non-terminals that are not defined.

@since 1.0.0
-}
data RefOutOfScopeException
    = RefOutOfScope Definition [NonTerminal]
    deriving (Show, Eq, Ord)

{-|
Exception raised when there are multiple definitions for the same non-terminal.

@since 1.0.0
-}
data DuplicateDefinitionException
    = DuplicateDefinition NonTerminal [Expression]
    deriving (Show, Eq, Ord)

{-|
Exceptions related to the semantic analysis of PEGs.

Possible exceptions include:
- 'OutOfScope': References to non-terminals out of scope.
- 'LeftRecursive': Left recursion detected.
- 'StarNullable': Nullable expressions inside repetition operators (`*`).
- 'DuplicateRule': Duplicate rules for the same non-terminal.

@since 1.0.0
-}
data PegException
    = OutOfScope [RefOutOfScopeException]
    | LeftRecursive [Definition]
    | StarNullable [Definition]
    | DuplicateRule [DuplicateDefinitionException]
    deriving (Show, Eq, Ord)

instance Exception RefOutOfScopeException
instance Exception DuplicateDefinitionException
instance Exception PegException

-------------------------------------------------------------------------------
--- Pretty Instances

{-|
Instance of the 'Pretty' class for 'PegException'.

Prints the exception in a readable format, detailing the type of error and the elements involved.

@since 1.0.0
-}
instance Pretty PegException where
    pPrint :: PegException -> Doc
    pPrint (OutOfScope refs) = pPrint refs
    pPrint (LeftRecursive defs) = text "The following definitions cause left recursion:"
                                    <+> pPrint defs
    pPrint (StarNullable defs) = text "The following definitions have nullable expressions inside *:"
                                    <+> pPrint defs
    pPrint (DuplicateRule defs) = pPrint defs

{-|
Instance of the 'Pretty' class for 'RefOutOfScopeException'.

Prints the exception indicating the rule that references non-terminals out of scope.

@since 1.0.0
-}
instance Pretty RefOutOfScopeException where
    pPrint :: RefOutOfScopeException -> Doc
    pPrint (RefOutOfScope def nts) = text "The rule" <+> pPrint def
                                     <+> text "depends on undefined rules"
                                     <+> parens (pPrint nts)

{-|
Instance of the 'Pretty' class for 'DuplicateDefinitionException'.

Prints the exception indicating the non-terminal with multiple definitions.

@since 1.0.0
-}
instance Pretty DuplicateDefinitionException where
    pPrint :: DuplicateDefinitionException -> Doc
    pPrint (DuplicateDefinition nt es) = text "Multiple definitions for " <+> pPrint nt
                                         <+> parens (pPrint es)

-------------------------------------------------------------------------------

{-|
Replaces the special symbol `.` in a PEG ('Grammar').

The symbol `.` is replaced by a choice of all terminals present in the grammar.

@since 1.0.0
-}
processDot :: Grammar -> Grammar
processDot g = first (map processDef) g
    where
        t = foldr1 Choice $ map ExprT $ terminals g
        processDef = second (everywhere $ mkT (changeNT t))
        changeNT ts e@(ExprNT (NT nt)) = if nt == "." then ts else e
        changeNT _  e                  = e

{-|
Checks for duplicate definitions in a PEG ('Grammar').

If there are multiple definitions for the same non-terminal, returns an exception
('DuplicateRule'). Otherwise, returns the original PEG.

@since 1.0.0
-}
duplicates :: Grammar -> Either PegException Grammar
duplicates g@(defs, _) =
    case duplicatesOfFirst defs of
        [] -> Right g
        ds -> Left . DuplicateRule $ map (\ x -> DuplicateDefinition x (filterByFirst defs x)) ds

{-|
Determines if a definition in a PEG ('Grammar') is nullable.

A definition is nullable if it can produce the empty string.

@since 1.0.0
-}
nullable :: Grammar -> Definition -> Bool
nullable _ (_, Empty)           = True
nullable _ (_, Star _)          = True
nullable _ (_, Not _)           = True
nullable _ (_, ExprT _)         = False
nullable g (nt, ExprNT nt')     = nt == nt'
                                  || nullable g (nt', fromMaybe (error $ "Error " ++ show nt') (expression g nt'))
nullable g (nt, Sequence e1 e2) = nullable g (nt, e1) && nullable g (nt, e2)
nullable g (nt, Choice e1 e2)   = nullable g (nt, e1) || nullable g (nt, e2)
nullable g (nt, Flatten e)      = nullable g (nt, e)
nullable g (nt, Indent e b)     = nullable g (nt, e) && nullable g (nt, b)

{-|
Identifies nullable expressions inside repetition operators (`*`, 'Star').

Returns a list of definitions that have nullable expressions inside repetition operators.

@since 1.0.0
-}
starNullable :: Grammar -> Definition -> [Definition]
starNullable _ (_, Empty)           = []
starNullable _ (_, ExprT _)         = []
starNullable g (nt, Not e)          = starNullable g (nt, e)
starNullable g (nt, Flatten e)      = starNullable g (nt, e)
starNullable g (nt, Choice e1 e2)   = starNullable g (nt, e1) ++ starNullable g (nt, e2)
starNullable g (nt, ExprNT nt')     = if nt == nt'
                                        then [(nt, ExprNT nt')]
                                        else starNullable g (nt', fromJust $ expression g nt')
starNullable g (nt, Sequence e1 e2) = if nullable g (nt, e1)
                                        then starNullable g (nt, e1) ++ starNullable g (nt, e2)
                                        else starNullable g (nt, e1)
starNullable g (nt, Star e)         = if nullable g (nt, e)
                                        then [(nt, Star e)]
                                        else starNullable g (nt, e)
starNullable g (nt, Indent e1 e2)   = if nullable g (nt, e1)
                                        then starNullable g (nt, e1) ++ starNullable g (nt, e2)
                                        else starNullable g (nt, e1)

{-|
Checks for nullable expressions inside repetition operators in a PEG ('Grammar').

If there are, returns an exception ('StarNullable'). Otherwise, returns `Nothing`.

@since 1.0.0
-}
recursiveLoop :: Grammar -> Maybe PegException
recursiveLoop g@(ds, _) =
    case expr of
        [] -> Nothing
        xs -> Just $ StarNullable xs
    where
        expr = nub $ concatMap (starNullable g) ds

{-|
Returns the non-terminals referenced by a definition in a PEG ('Grammar').

@since 1.0.0
-}
referencesNull :: Grammar -> Definition -> [NonTerminal]
referencesNull _ (_, Empty)           = []
referencesNull _ (_, ExprT _)         = []
referencesNull _ (_, ExprNT nt')      = [nt']
referencesNull g (nt, Sequence e1 e2) = if nullable g (nt, e1)
                                        then referencesNull g (nt, e1) ++ referencesNull g (nt, e2)
                                        else referencesNull g (nt, e1)
referencesNull g (nt, Choice e1 e2)   = referencesNull g (nt, e1) ++ referencesNull g (nt, e2)
referencesNull g (nt, Star e)         = referencesNull g (nt, e)
referencesNull g (nt, Not e)          = referencesNull g (nt, e)
referencesNull g (nt, Flatten e)      = referencesNull g (nt, e)
referencesNull g (nt, Indent e1 e2)   = if nullable g (nt, e1)
                                        then referencesNull g (nt, e1) ++ referencesNull g (nt, e2)
                                        else referencesNull g (nt, e1)

{-|
Calculates the dependencies of a definition in a PEG ('Grammar').

Returns a list of pairs of definitions representing the dependencies. If there are
references to non-terminals out of scope, returns an exception ('RefOutOfScopeException').

@since 1.0.0
-}
dependencies ::
    Grammar
    -> Definition
    -> Either RefOutOfScopeException [(Definition, Definition)] -- List of dependencies
dependencies g@(ds, _) d =
    case filter (isNothing . snd) depends of
        [] -> Right $ map (\ x -> (second fromJust x, d)) depends
        x -> Left $ RefOutOfScope d (map fst x)
    where
        refs = nub $ referencesNull g d
        findNTs ds' s = (s, lookup s ds')
        depends = map (findNTs ds) refs

{-|
Creates the edges of a dependency graph for a PEG ('Grammar').

If there are references out of scope, returns an exception ('OutOfScope').

@since 1.0.0
-}
mkEdges :: Grammar -> Either PegException [(Definition, Definition)]
mkEdges g@(ds, _) =
    case lefts result of
        [] -> Right . concat . rights $ result
        p -> Left . OutOfScope $ p
    where
        result = map (dependencies g) ds

{-|
Creates a dependency graph for a PEG ('Grammar').

If there are duplicate definitions or references out of scope, returns an exception.

@since 1.0.0
-}
mkGraph :: Grammar -> Either PegException (Alga.AdjacencyMap Definition)
mkGraph g@(ds, _) =
    case duplicates g of
        Left e -> Left e
        Right ds' -> second (Alga.overlay (Alga.vertices ds) . Alga.edges) (mkEdges ds')

{-|
Checks for left recursion in a PEG ('Grammar').

If there is, returns an exception ('LeftRecursive'). Otherwise, returns `Nothing`.

@since 1.0.0
-}
leftRecursive :: Alga.AdjacencyMap Definition -> Maybe PegException
leftRecursive g = leftToMaybe (first (LeftRecursive . toList) (Algo.topSort g))
    where
        leftToMaybe = either Just (const Nothing)

{-|
Processes and validates a PEG ('Grammar').

The 'processPeg' function performs the following validations:
1. Replaces the special symbol `.` with a choice of all terminals.
2. Checks for duplicate rules.
3. Detects left recursion.
4. Identifies nullable expressions inside repetition operators (`*`, 'Star').

If the grammar is valid, returns the processed grammar. Otherwise, returns
an exception ('PegException') indicating the error found.

=== Usage examples:

>>> let grammar = ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S")
>>> processPeg grammar
Right ([(NT "S",Sequence (ExprT (T "a")) (ExprT (T "b")))],NT "S")

>>> let invalidGrammar = ([(NT "S", Sequence (ExprNT (NT "S")) (ExprT (T "a")))], NT "S")
>>> processPeg invalidGrammar
Left (LeftRecursive [(NT "S",Sequence (ExprNT (NT "S")) (ExprT (T "a")))])

@since 1.0.0
-}
processPeg :: Grammar -> Either PegException Grammar
processPeg g =
    case mkGraph g' of
        Left x -> Left x
        Right graph ->
            case leftRecursive graph of
                Just x -> Left x
                Nothing -> maybe (Right g') Left (recursiveLoop g')
    where
        g' = processDot g
