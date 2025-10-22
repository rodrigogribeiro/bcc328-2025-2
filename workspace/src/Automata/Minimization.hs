module Automata.Minimization where 

import Automata.DFA 
import Automata.NFA 
import Data.Set (Set)
import qualified Data.Set as Set 


-- reachable states from a DFA 

reachable :: Ord a => DFA a -> [Char] -> [a]
reachable (DFA s d _) sigma 
  = Set.toList (go s Set.empty)
  where 
    go state visited 
      | state `Set.member` visited = visited 
      | otherwise 
          = let 
              visited' = Set.insert state visited 
              nexts = map (d state) sigma 
            in foldr go visited' nexts 

reverseDFA :: Ord a => DFA a -> [a] -> NFA a
reverseDFA (DFA s d f) allStates =
    let
        reversedStart = Set.fromList [s' | s' <- allStates, f s']
        reversedDelta state c =
            Set.fromList [s' | s' <- allStates, d s' c == state]
        reversedFinals = Set.singleton s 
    in NFA (length allStates) reversedStart reversedDelta reversedFinals

-- minimization 

brzozowski :: Ord a => DFA a -> [Char] -> [a] -> DFA (Set (Set a))
brzozowski m sigma allStates 
  = let 
      m1 = subset (reverseDFA m allStates)
      m2 = reverseDFA m1 (reachable m1 sigma)
    in subset m2
