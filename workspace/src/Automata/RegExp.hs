module Automata.RegExp ( Regex (..)
                       , emptyNFA
                       , lambdaNFA
                       , chrNFA
                       , unionNFA
                       , concatNFA
                       , starNFA
                       , thompson
                       , toDFA
                       , lexer
                       ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Automata.NFA
import Automata.DFA

-- definition of regular expressions

data Regex
  = Empty
  | Lambda
  | Chr Char
  | Regex :+: Regex
  | Regex :@: Regex
  | Star Regex
  deriving (Eq, Ord, Show)

-- Thompson construction

emptyNFA :: NFA Int
emptyNFA
  = NFA 0 Set.empty (\ _ _ -> Set.empty) Set.empty

lambdaNFA :: NFA Int
lambdaNFA
  = NFA 1 one (\ _ _ -> Set.empty) one
    where
      one = Set.singleton 1

chrNFA :: Char -> NFA Int
chrNFA c
  = NFA 2 zero f one
    where
      zero = Set.singleton 0
      one = Set.singleton 1
      err = Set.singleton 2
      f 0 x = if c == x then one
              else err
      f _ _ = err

shift :: Int -> Set Int -> Set Int
shift n = Set.fromAscList . map (+ n) . Set.toAscList

unionNFA :: NFA Int -> NFA Int -> NFA Int
unionNFA m1 m2
  = NFA {
      numberOfStates = n1 + n2
    , nfaStart = Set.union (nfaStart m1) (shift n1 (nfaStart m2))
    , nfaDelta = f
    , nfaFinals = Set.union (nfaFinals m2) (shift n1 (nfaFinals m2))
    }
    where
      n1 = numberOfStates m1
      n2 = numberOfStates m2
      f s c = if s < n1 then nfaDelta m1 s c
              else shift n1 (nfaDelta m2 (s - n1) c)

concatNFA :: NFA Int -> NFA Int -> NFA Int
concatNFA m1 m2
  = NFA {
      numberOfStates = n1 + n2
    , nfaStart = newStart
    , nfaDelta = newDelta
    , nfaFinals = newFinals
    }
    where
      n1 = numberOfStates m1
      n2 = numberOfStates m2
      start1 = nfaStart m1
      final1 = nfaFinals m1
      newStart = if disjoint start1 final1
                 then start1
                 else Set.union start1 (shift n1 final1)
      newFinals = shift n1 final1
      newDelta e c = if e < n1 then
                       if disjoint (nfaDelta m1 e c) final1
                       then nfaDelta m1 e c
                       else Set.union (nfaDelta m1 e c) start1
                     else shift n1 (nfaDelta m2 (e - n1) c)

starNFA :: NFA Int -> NFA Int
starNFA m1
  = NFA {
      numberOfStates = numberOfStates m1
    , nfaStart = nfaStart m1
    , nfaDelta = newDelta
    , nfaFinals = nfaStart m1
    }
    where
      newDelta e c
        = let r = nfaDelta m1 e c
          in if disjoint r (nfaFinals m1)
             then r
             else Set.union r (nfaStart m1)


thompson :: Regex -> NFA Int
thompson Empty = emptyNFA
thompson Lambda = lambdaNFA
thompson (Chr c) = chrNFA c
thompson (e1 :+: e2)
  = unionNFA (thompson e1) (thompson e2)
thompson (e1 :@: e2)
  = concatNFA (thompson e1) (thompson e2)
thompson (Star e1)
  = starNFA (thompson e1)

toDFA :: Regex -> DFA (Set Int)
toDFA = subset . thompson

lexer :: [Regex] -> DFA (Set Int)
lexer = subset . foldr unionNFA emptyNFA . map thompson
