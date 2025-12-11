module Parsing.SLR where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

import Parsing.Grammar
import Parsing.First
import Parsing.Follow
import Parsing.LR0 (Item(..), State, Action(..), ParsingTable(..),
                     initialItem, closure, goto, nextSymbols,
                     findStateIndex, buildStates, parseLR0,
                     printParsingTable, fixedPointSet)

-- ============================================================================
-- SLR Parsing Table Construction
-- ============================================================================

-- Build the SLR parsing table from states
-- The key difference from LR(0): reduce actions only for tokens in FOLLOW set
buildSLRParsingTable :: Grammar -> FirstSet -> FollowSet -> [State] -> ParsingTable
buildSLRParsingTable g fs follows states = ParsingTable actions gotos states
  where
    g' = Map.insert "S'" [[NonTerminal startSym]] g
    startSym = head (nonTerminals g)

    actions = Map.fromList $ concat
        [ buildActions i state | (i, state) <- zip [0..] states ]

    gotos = Map.fromList $ concat
        [ buildGotos i state | (i, state) <- zip [0..] states ]

    buildActions i state = shiftActions ++ reduceActions ++ acceptAction
      where
        -- Shift actions: for each terminal after dot
        shiftActions =
            [ ((i, symbolString t), Shift j)
            | Terminal t <- nextSymbols state
            , not (isLambda (Terminal t))
            , let nextState = goto g' state (Terminal t)
            , Just j <- [findStateIndex states nextState]
            ]

        -- SLR Reduce actions: for items with dot at end,
        -- but ONLY for terminals in FOLLOW(LHS)
        reduceActions =
            [ ((i, t), Reduce lhs alpha)
            | item <- Set.toList state
            , null (itemAfter item)
            , let lhs = itemLHS item
            , let alpha = itemBefore item
            , lhs /= "S'"  -- Don't reduce S' productions
            , t <- fromMaybe [] (Map.lookup lhs follows)  -- Only FOLLOW(lhs)
            ]

        -- Accept action: S' -> S . with $ as lookahead
        acceptAction =
            [ ((i, "$"), Accept)
            | item <- Set.toList state
            , itemLHS item == "S'"
            , null (itemAfter item)
            ]

    buildGotos i state =
        [ ((i, symbolString nt), j)
        | NonTerminal nt <- nextSymbols state
        , let nextState = goto g' state (NonTerminal nt)
        , Just j <- [findStateIndex states nextState]
        ]

-- Main function to construct SLR parsing table
constructSLRTable :: Grammar -> ParsingTable
constructSLRTable g = buildSLRParsingTable g firstSets followSets states
  where
    states = buildStates g
    firstSets = computeFirstSets g
    followSets = computeFollowSets g firstSets

-- Parsing function (reuses LR(0) parsing algorithm)
parseSLR :: Grammar -> ParsingTable -> [String] -> ParseResult
parseSLR = parseLR0

-- Convenience type for parse results
type ParseResult = Either String ()

-- Check for conflicts in the parsing table
data Conflict
    = ShiftReduceConflict Int String Action Action
    | ReduceReduceConflict Int String Action Action
    deriving (Show)

checkConflicts :: ParsingTable -> [Conflict]
checkConflicts table = Map.foldrWithKey checkEntry [] grouped
  where
    -- Group actions by (state, terminal) key
    actionList = Map.toList (actionTable table)
    grouped = Map.fromListWith (++)
        [ (key, [action]) | (key, action) <- actionList ]

    checkEntry key actions conflicts
        | length actions > 1 = detectConflictType key actions ++ conflicts
        | otherwise = conflicts

    detectConflictType (state, term) actions =
        let sorted = sort actions
        in case sorted of
            [Shift _, Reduce _ _] ->
                [ShiftReduceConflict state term (sorted !! 0) (sorted !! 1)]
            [Reduce _ _, Reduce _ _] ->
                [ReduceReduceConflict state term (sorted !! 0) (sorted !! 1)]
            _ -> []

-- Print conflicts
printConflicts :: [Conflict] -> IO ()
printConflicts [] = putStrLn "No conflicts detected."
printConflicts conflicts = do
    putStrLn "\n=== Conflicts Detected ==="
    mapM_ printConflict conflicts
  where
    printConflict (ShiftReduceConflict state term a1 a2) =
        putStrLn $ "Shift-Reduce conflict in state " ++ show state
                ++ " on '" ++ term ++ "': "
                ++ show a1 ++ " vs " ++ show a2
    printConflict (ReduceReduceConflict state term a1 a2) =
        putStrLn $ "Reduce-Reduce conflict in state " ++ show state
                ++ " on '" ++ term ++ "': "
                ++ show a1 ++ " vs " ++ show a2

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- Pretty print SLR table with conflict detection
printSLRTable :: ParsingTable -> IO ()
printSLRTable table = do
    printParsingTable table
    putStrLn ""
    let conflicts = checkConflicts table
    printConflicts conflicts

-- Complete parsing with error handling
parseWithTable :: Grammar -> [String] -> IO ()
parseWithTable g tokens = do
    let table = constructSLRTable g
    putStrLn $ "\nParsing input: " ++ show tokens
    case parseSLR g table tokens of
        Right () -> putStrLn "✓ Parse successful!"
        Left err -> putStrLn $ "✗ Parse error: " ++ err
