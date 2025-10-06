module Automata.ExamplesDFA ( numberDFA
                            , ifDFA
                            , identDFA
                            , ifOrIdentDFA
                            ) where

import Data.Char

import Automata.DFA


-- example dfas

numberDFA :: DFA (Maybe Bool)
numberDFA
  = DFA {
      start = Just False
    , delta = numberTrans
    , finals = \ e -> e == Just True
    }
    where
      numberTrans (Just False) c
        | isDigit c = Just True
        | otherwise = Nothing
      numberTrans (Just True) c
        | isDigit c = Just True
        | otherwise = Nothing
      numberTrans _ _ = Nothing
      
-- recognizing if keyword

ifDFA :: DFA (Maybe Int)
ifDFA
  = DFA {
      start = Just 0
    , delta = ifTrans
    , finals = \ e -> e == Just 2
    }
 where
   ifTrans (Just 0) 'i' = Just 1
   ifTrans (Just 1) 'f' = Just 2
   ifTrans _ _ = Nothing

-- recognizing identifiers

identDFA :: DFA (Maybe Int)
identDFA
  = DFA {
      start = Just 0
    , delta = identTrans
    , finals = \ e -> e == Just 1
    }
  where
    identTrans (Just 0) c
      | isLetter c = Just 1
      | otherwise = Nothing
    identTrans (Just 1) c
      | isAlphaNum c = Just 1
      | otherwise = Nothing
    identTrans _ _ = Nothing

-- recognizing if or identifiers

ifOrIdentDFA :: DFA (Maybe Int, Maybe Int)
ifOrIdentDFA = unionDFA ifDFA identDFA
 
