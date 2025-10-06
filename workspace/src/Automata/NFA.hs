module Automata.NFA ( NFA (..)
                    , nfaDeltaStar
                    , nfaAccept
                    , subset
                    , disjoint
                    ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Automata.DFA

-- definition of a NFA

data NFA a
  = NFA {
      numberOfStates :: Int
    , nfaStart :: Set a
    , nfaDelta :: a -> Char -> Set a
    , nfaFinals :: Set a
    }

-- running a NFA

nfaDeltaStar :: Ord a => NFA a -> String -> Set a
nfaDeltaStar m = deltaStar' (nfaStart m)
  where
    deltaStar' es []
      = es
    deltaStar' es (x : xs)
      = deltaStar' (Set.unions (map (flip (nfaDelta m) x) (Set.elems es))) xs


nfaAccept :: Ord a => NFA a -> String -> Bool
nfaAccept m s
  = not (Set.null $ Set.intersection (nfaDeltaStar m s) (nfaFinals m))

-- subset construction

subset :: Ord a => NFA a -> DFA (Set a)
subset m
  = DFA {
      start  = nfaStart m
    , delta  = \ es c ->
        Set.unions (map (flip (nfaDelta m) c) (Set.elems es))
    , finals = \ es -> not (disjoint es (nfaFinals m))
    }

-- checking if two sets are disjoint

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint s1 s2
  = Set.null (Set.intersection s1 s2)
