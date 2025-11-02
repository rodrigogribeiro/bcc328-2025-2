module Parsing.LL.BuildTable where 

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Parsing.First 
import Parsing.Follow 
import Parsing.Grammar


-- Parser table and actions
type ParseTable = Map (String, String) ParseAction

data ParseAction = Derive Production | Accept | Error
    deriving (Show, Eq)

-- table construction

buildParseTable :: Grammar -> ParseTable
buildParseTable g
  = foldl (addProductions g) (initialTable g) (allProductions g)

allProductions :: Grammar -> [(String, [Symbol])]
allProductions grammar 
  = [(lhs, prod) | (lhs, prods) <- Map.toList grammar, prod <- prods]

initialTable :: Grammar -> ParseTable 
initialTable grammar 
  = Map.fromList [((nt, t), Error) 
                 | nt <- nonTerminals grammar
                 , t <- terminals grammar ++ ["$"]]
    

addProductions :: Grammar -> ParseTable -> (String, Production) -> ParseTable
addProductions grammar table (lhs, prod)
    = foldl (addToTable lhs prod firstAlpha) table firstAlpha
      where
        firstAlpha = firstForSequence grammar firstSets prod
        firstSets = computeFirstSets grammar 
        followSets = computeFollowSets grammar firstSets
        addToTable lhs' prod' _ table' a
            | a /= "λ" =
                case Map.lookup (lhs', a) table' of
                    Just Error -> Map.insert (lhs', a) (Derive prod') table'
                    Just _ -> error $ "Grammar is not LL(1)! Conflict at (" ++ lhs' ++ ", " ++ a ++ ")"
                    Nothing -> table'
            | otherwise =
                let followLhs = fromMaybe [] (Map.lookup lhs' followSets)
                in foldl (addLamProduction lhs' prod') table' followLhs
        
        addLamProduction :: String -> Production -> ParseTable -> String -> ParseTable
        addLamProduction lhs' prod' table' b =
            case Map.lookup (lhs', b) table' of
                Just Error -> Map.insert (lhs', b) (Derive prod') table'
                Just _ -> error $ "Grammar is not LL(1)! Conflict at (" ++ lhs' ++ ", " ++ b ++ ")"
                Nothing -> table'

printParseTable :: Grammar -> IO ()
printParseTable grammar = do
    putStrLn "LL(1) Parse Table:"
    let table = buildParseTable grammar
        nonTerms = nonTerminals grammar
        terms = terminals grammar ++ ["$"]
    putStrLn $ "     " ++ unwords (map (\t -> pad 8 t) terms)
    mapM_ (\nt -> do
        putStr $ pad 5 nt
        mapM_ (\t -> 
            putStr $ pad 8 $ case Map.lookup (nt, t) table of
                Just (Derive prod) -> showProduction prod
                Just Accept -> "acc"
                Just Error -> "-"
                Nothing -> ""
            ) terms
        putStrLn "") nonTerms
  where
    pad n s = take n (s ++ replicate n ' ')
    showProduction prod = unwords (map symbolString prod)
