{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, InstanceSigs #-}
{-|
Module      : Syntax.Base
Description : Basic definitions for symbols and grammar utilities.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the basic types to represent terminal and non-terminal symbols
in a grammar, as well as utilities for list manipulation and a class for formatted printing.
-}
module PEG.Pattern.Syntax.Base
    ( NonTerminal(..)
    , Terminal(..)
    , Symbol
    , Pretty(..)
    , toMaybe
    , duplicatesOfFirst
    , filterByFirst
    ) where

import Text.PrettyPrint.HughesPJ (text, Doc, hcat)
import Data.Generics (Data, Typeable)

{-|
Represents a non-terminal symbol in a grammar.

A 'NonTerminal' is simply a string that identifies the non-terminal.

@since 1.0.0
-}
data NonTerminal
    = NT String
    deriving (Eq, Show, Ord, Typeable, Data)

{-|
Represents a terminal symbol in a grammar.

A 'Terminal' is a string that identifies the terminal.

@since 1.0.0
-}
data Terminal
    = T String
    deriving (Eq, Show, Ord, Typeable, Data)

{-|
Represents a "symbol," which is either a 'NonTerminal' or a 'Terminal'.

@since 1.0.0
-}
type Symbol = Either NonTerminal Terminal

{-|
Type class for types that can be printed as a text document (PrettyPrint).

The 'Pretty' class defines the 'pPrint' method to generate the representation in 'Doc' format.

@since 1.0.0
-}
class Pretty a where
    {-|
    Generates the formatted representation of a value as a 'Doc'.

    @since 1.0.0
    -}
    pPrint :: a -> Doc

{-|
Instance of the 'Pretty' class for lists.

Prints each element of the list concatenated.

@since 1.0.0
-}
instance Pretty a => Pretty [a] where
    pPrint :: [a] -> Doc
    pPrint l = hcat (map pPrint l)

{-|
Instance of the 'Pretty' class for 'NonTerminal'.

Prints the name of the non-terminal.

@since 1.0.0
-}
instance Pretty NonTerminal where
    pPrint :: NonTerminal -> Doc
    pPrint (NT nt) = text nt

{-|
Instance of the 'Pretty' class for 'Terminal'.

Prints the name of the terminal in quotes.

@since 1.0.0
-}
instance Pretty Terminal where
    pPrint :: Terminal -> Doc
    pPrint (T t) = text (show t)

{-|
Instance of the 'Pretty' class for 'Symbol'.

Prints the symbol, whether it is a 'NonTerminal' or a 'Terminal'.

@since 1.0.0
-}
instance Pretty Symbol where
    pPrint :: Symbol -> Doc
    pPrint (Left nt) = pPrint nt
    pPrint (Right t) = pPrint t

{-|
The 'toMaybe' function takes a boolean value and a generic value.

If the boolean value is 'True', it returns the value wrapped in a 'Just'.
Otherwise, it returns 'Nothing'.

=== Usage examples:

>>> toMaybe True "Hello"
Just "Hello"

>>> toMaybe False "Hello"
Nothing

@since 1.0.0
-}
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a

{-|
The 'duplicatesOfFirst' function takes a list of pairs @(a, b)@ and returns a list containing
the elements @a@ that appear more than once in the list. The function compares only the first
elements @(a)@ of the pairs.

=== Usage examples:

>>> duplicatesOfFirst [(1, "a"), (2, "b"), (1, "c"), (3, "d"), (2, "e")]
[2,1]

@since 1.0.0
-}
duplicatesOfFirst :: Eq a => [(a, b)] -> [a]
duplicatesOfFirst ls = duplicates ls [] []
    where
        duplicates []     _       dups = dups
        duplicates (x:xs) checked dups =
            if fst x `elem` checked
                then duplicates xs checked (fst x:dups)
                else duplicates xs (fst x:checked) dups

{-|
The 'filterByFirst' function takes a list of pairs @(a, b)@ and a value of type @a@.

It returns a list containing all the @b@ values that are associated with the given @a@
value. The function filters the pairs by the key @a@ and returns the corresponding values.

=== Usage examples:

>>> filterByFirst [(1, "a"), (2, "b"), (1, "c"), (3, "d")] 1
["a","c"]

@since 1.0.0
-}
filterByFirst :: Eq a => [(a, b)] -> a -> [b]
filterByFirst g' x = map snd $ filter ((x ==) . fst) g'
