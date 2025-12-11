module Parsing.LR.LR0.BuildTable where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

import Parsing.Grammar
import Utils.Fixpoint


-- LR(0) Item: A -> alpha . beta
data Item = Item
    { itemLHS :: String           -- Left-hand side non-terminal
    , itemBefore :: [Symbol]      -- Symbols before the dot
    , itemAfter :: [Symbol]       -- Symbols after the dot
    } deriving (Eq, Ord)

instance Show Item where
    show (Item lhs before after) =
        lhs ++ " -> " ++ showSyms before ++ " . " ++ showSyms after
      where
        showSyms = unwords . map symbolString

-- LR(0) State: A set of items
type State = Set Item

-- Action table entries
data Action
    = Shift Int       -- Shift and go to state n
    | Reduce String [Symbol]  -- Reduce by production A -> alpha
    | Accept          -- Accept the input
    deriving (Eq, Show)

-- Parsing table
data ParsingTable = ParsingTable
    { actionTable :: Map (Int, String) Action  -- (state, terminal) -> action
    , gotoTable :: Map (Int, String) Int       -- (state, non-terminal) -> state
    , states :: [State]                        -- All states
    } deriving (Show)

-- ============================================================================
-- PART 1: Constructing LR(0) Items and Parsing Table
-- ============================================================================

-- Create initial item for start symbol: S' -> . S
initialItem :: Grammar -> Item
initialItem g = Item "S'" [] [NonTerminal startSym]
  where
    startSym = head (nonTerminals g)

-- Compute closure of a set of items
closure :: Grammar -> State -> State
closure g items = fixedPoint step items
  where
    step current = Set.union current newItems
      where
        newItems = Set.fromList $ concatMap expand (Set.toList current)

        expand item = case itemAfter item of
            (NonTerminal b : _) ->
                case Map.lookup b g of
                    Just prods -> [Item b [] prod | prod <- prods]
                    Nothing -> []
            _ -> []

-- Compute goto(I, X) - items obtained by shifting X
goto :: Grammar -> State -> Symbol -> State
goto g items sym = closure g $ Set.fromList shifted
  where
    shifted = mapMaybe shiftItem (Set.toList items)

    shiftItem item = case itemAfter item of
        (s:rest) | s == sym ->
            Just $ Item (itemLHS item)
                        (itemBefore item ++ [s])
                        rest
        _ -> Nothing

-- Get all symbols that can follow the dot in a state
nextSymbols :: State -> [Symbol]
nextSymbols state = nub $ mapMaybe getNext (Set.toList state)
  where
    getNext item = case itemAfter item of
        (s:_) -> Just s
        [] -> Nothing

-- Build canonical collection of LR(0) states
buildStates :: Grammar -> [State]
buildStates g = buildFrom [initial] [initial]
  where
    initial = closure g (Set.singleton $ initialItem g')
    -- Augment grammar with S' -> S
    g' = Map.insert "S'" [[NonTerminal startSym]] g
    startSym = head (nonTerminals g)

    buildFrom seen [] = seen
    buildFrom seen (current:queue) =
        let symbols = nextSymbols current
            newStates = [goto g' current sym | sym <- symbols]
            unseen = filter (`notElem` seen) newStates
        in buildFrom (seen ++ unseen) (queue ++ unseen)

-- Find state index in list of states
findStateIndex :: [State] -> State -> Maybe Int
findStateIndex states' state = findIndex (== state) states'

-- Build the parsing table from states
buildParsingTable :: Grammar -> [State] -> ParsingTable
buildParsingTable g states' = ParsingTable actions gotos states'
  where
    g' = Map.insert "S'" [[NonTerminal startSym]] g
    startSym = head (nonTerminals g)

    actions = Map.fromList $ concat
        [ buildActions i state | (i, state) <- zip [0..] states' ]

    gotos = Map.fromList $ concat
        [ buildGotos i state | (i, state) <- zip [0..] states' ]

    buildActions i state = shiftActions ++ reduceActions ++ acceptAction
      where
        -- Shift actions: for each terminal after dot
        shiftActions =
            [ ((i, t), Shift j)
            | Terminal t <- nextSymbols state
            , not (isLambda (Terminal t))
            , let nextState = goto g' state (Terminal t)
            , Just j <- [findStateIndex states' nextState]
            ]

        -- Reduce actions: for items with dot at end
        reduceActions =
            [ ((i, t), Reduce lhs alpha)
            | item <- Set.toList state
            , null (itemAfter item)
            , let lhs = itemLHS item
            , let alpha = itemBefore item
            , lhs /= "S'"  -- Don't reduce S' productions
            , t <- terminals g ++ ["$"]
            ]

        -- Accept action: S' -> S .
        acceptAction =
            [ ((i, "$"), Accept)
            | item <- Set.toList state
            , itemLHS item == "S'"
            , null (itemAfter item)
            ]

    buildGotos i state =
        [ ((i, nt), j)
        | NonTerminal nt <- nextSymbols state
        , let nextState = goto g' state (NonTerminal nt)
        , Just j <- [findStateIndex states' nextState]
        ]

-- Main function to construct LR(0) parsing table
constructLR0Table :: Grammar -> ParsingTable
constructLR0Table g = buildParsingTable g (buildStates g)

-- Print parsing table
printParsingTable :: ParsingTable -> IO ()
printParsingTable (ParsingTable actions gotos states') = do
    putStrLn "\n=== LR(0) States ==="
    mapM_ printState (zip [0 :: Int ..] states')

    putStrLn "\n=== Action Table ==="
    mapM_ printAction (Map.toList actions)

    putStrLn "\n=== Goto Table ==="
    mapM_ printGoto (Map.toList gotos)
  where
    printState (i, state) = do
        putStrLn $ "State " ++ show i ++ ":"
        mapM_ (putStrLn . ("  " ++) . show) (Set.toList state)

    printAction ((state, term), action) =
        putStrLn $ "  [" ++ show state ++ ", " ++ term ++ "] = " ++ show action

    printGoto ((state, nt), dest) =
        putStrLn $ "  [" ++ show state ++ ", " ++ nt ++ "] = " ++ show dest
