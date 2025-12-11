module Parsing.LR.LR0.LR0Parser where

import qualified Data.Map as Map

import Parsing.Grammar
import Parsing.LR.LR0.BuildTable

-- ============================================================================
-- PART 2: LR(0) Parsing Algorithm
-- ============================================================================

-- Parser state
data ParserState = ParserState
    { stateStack :: [Int]         -- Stack of states
    , symbolStack :: [Symbol]     -- Stack of symbols
    , inputBuffer :: [String]     -- Remaining input
    } deriving (Show)

-- Parse result
data ParseResult
    = ParseSuccess
    | ParseError String
    deriving (Show)

-- Perform one step of LR parsing
parseStep :: Grammar -> ParsingTable -> ParserState -> Either String ParserState
parseStep g table ps@(ParserState states' syms input) =
    case input of
        [] -> Left "Unexpected end of input"
        (token:rest) ->
            let currentState = head states'
                action = Map.lookup (currentState, token) (actionTable table)
            in case action of
                Nothing -> Left $ "No action for state " ++ show currentState
                                ++ " and token " ++ token

                Just (Shift n) ->
                    Right $ ParserState
                        (n : states')
                        (Terminal token : syms)
                        rest

                Just (Reduce lhs rhs) ->
                    let k = length rhs
                        states'' = drop k states'
                        syms' = drop k syms
                        currentState' = head states''
                        gotoState = Map.lookup (currentState', lhs) (gotoTable table)
                    in case gotoState of
                        Nothing -> Left $ "No goto for state " ++ show currentState'
                                        ++ " and non-terminal " ++ lhs
                        Just n -> parseStep g table $ ParserState
                            (n : states'')
                            (NonTerminal lhs : syms')
                            input  -- Don't consume input on reduce

                Just Accept -> Right ps  -- Signal acceptance

-- Main parsing function
parseLR0 :: Grammar -> ParsingTable -> [String] -> ParseResult
parseLR0 g table tokens = go initialState
  where
    initialState = ParserState [0] [] (tokens ++ ["$"])

    go ps = case parseStep g table ps of
        Left err -> ParseError err
        Right ps'@(ParserState states' _ input) ->
            case (input, Map.lookup (head states', head input) (actionTable table)) of
                (["$"], Just Accept) -> ParseSuccess
                _ -> go ps'
