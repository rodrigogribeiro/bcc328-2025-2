module Parsing.Grammar where 

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Symbol = Terminal String | NonTerminal String
    deriving (Eq, Ord, Show)

type Production = [Symbol]

type Grammar = Map String [Production]

-- some functions over grammar types 

nonTerminals :: Grammar -> [String]
nonTerminals = Map.keys

terminals :: Grammar -> [String]
terminals grammar = nub $ concatMap terminalsFromProd $ 
    concatMap snd (Map.toList grammar)
  where
    terminalsFromProd :: Production -> [String]
    terminalsFromProd = mapMaybe $ \ x -> case x of 
        Terminal t -> if t /= "λ" then Just t else Nothing
        _ -> Nothing

symbolString :: Symbol -> String
symbolString (Terminal s) = s
symbolString (NonTerminal s) = s

isLambda :: Symbol -> Bool
isLambda (Terminal "λ") = True
isLambda _ = False

isTerminal :: Symbol -> Bool
isTerminal (Terminal _) = True
isTerminal _ = False

unions :: Eq a => [[a]] -> [a]
unions = foldr union []


