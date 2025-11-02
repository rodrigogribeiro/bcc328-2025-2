module Parsing.Follow where 

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Parsing.First 
import Parsing.Grammar
import Utils.Fixpoint

-- definition of follow sets 

type FollowSet = Map String [String]

-- Compute FOLLOW sets

computeFollowSets :: Grammar -> FirstSet -> FollowSet
computeFollowSets g fs 
  = fixedPoint step initial
    where
      startSymbol = head (nonTerminals g)
      initial = Map.fromList 
          [ (nt, if nt == startSymbol then ["$"] else []) 
          | nt <- nonTerminals g]
    
      step cur = foldl update cur allProductions
        where
          allProductions = [(nt, prod) | (nt, prods) <- Map.toList g
                                       , prod <- prods]
        
          update follow (lhs, prod) = 
            foldl (updatePos lhs) follow [0..length prod - 1]
            where
              updatePos lhs' follow' pos
                | NonTerminal b <- prod !! pos =
                    let beta = drop (pos + 1) prod
                        firstBeta = firstForSequence g fs beta
                        followB = fromMaybe [] (Map.lookup b follow')
                        
                        newFollow1 = if "λ" `elem` firstBeta
                                    then followB `union` 
                                         fromMaybe [] (Map.lookup lhs' cur)
                                    else followB
                        
                        newFollow2 = newFollow1 `union` (firstBeta \\ ["λ"]) 
                    in Map.insert b newFollow2 follow'
                | otherwise = follow'

-- outputing

printFollowSets :: Grammar -> IO ()
printFollowSets grammar = do
    putStrLn "FOLLOW Sets:"
    let firstSets = computeFirstSets grammar
        followSets = computeFollowSets grammar firstSets
    mapM_ (\(nt, follow) -> putStrLn $ "  FOLLOW(" ++ nt ++ ") = " ++ show follow) 
          (Map.toList followSets)
