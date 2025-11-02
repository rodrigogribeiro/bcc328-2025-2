module Parsing.First where 

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Parsing.Grammar
import Utils.Fixpoint

-- definition of first set 

type FirstSet = Map String [String]

-- Compute FIRST sets for all non-terminals
computeFirstSets :: Grammar -> FirstSet
computeFirstSets g 
  = fixedPoint step initial
    where
      initial = Map.fromList [(nt, []) | nt <- nonTerminals g]
    
      step cur = Map.mapWithKey update cur
        where
          update nt _ = unions $ map (firstForSequence g cur) 
                                     (fromMaybe [] (Map.lookup nt g))

-- first set for a sequence of symbols 

firstForSequence :: Grammar -> FirstSet -> [Symbol] -> [String]
firstForSequence _ _ [] = ["λ"]
firstForSequence g fs (sym:syms)
    | isTerminal sym = 
      if isLambda sym   
      then firstForSequence g fs syms
      else [symbolString sym]
    | otherwise =
        let nt = symbolString sym
            firstOfSym = fromMaybe [] (Map.lookup nt fs)
            firstWithoutLam = firstOfSym \\ ["λ"]
        in if "λ" `elem` firstOfSym
           then firstWithoutLam `union` firstForSequence g fs syms
           else firstWithoutLam

-- printing first sets 

printFirstSets :: Grammar -> IO ()
printFirstSets grammar = do
    putStrLn "FIRST Sets:"
    let firstSets = computeFirstSets grammar
    mapM_ (\(nt, first) -> putStrLn $ "  FIRST(" ++ nt ++ ") = " ++ show first) 
          (Map.toList firstSets)
