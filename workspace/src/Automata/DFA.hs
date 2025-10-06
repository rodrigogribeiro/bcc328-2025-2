module Automata.DFA ( DFA (..)
                    , deltaStar
                    , accept
                    , longest
                    , unionDFA
                    , intersectionDFA
                    ) where

-- definition of a deterministic finite automata

data DFA a
  = DFA {
      start :: a
    , delta :: a -> Char -> a
    , finals :: a -> Bool
    } 


-- producing the resulting state

deltaStar :: DFA a -> String -> a
deltaStar m = foldl (delta m) (start m)

-- checking membership

accept :: DFA a -> String -> Bool
accept m s = finals m (deltaStar m s) 

-- longest match

longest :: DFA a -> String -> Maybe String
longest m = combine . foldl step (Just "", Nothing, start m)
  where
    step (Just pre, Nothing, e) c
      | finals m (delta m e c)
          = ( Just (c : pre)
            , Just (c : pre)
            , delta m e c)
      | otherwise
          = ( Just (c : pre)
            , Nothing
            , delta m e c)
    step (Just pre, Just pre', e) c
      | finals m (delta m e c)
          = ( Just (c : pre)
            , Just (c : pre')
            , delta m e c)
      | otherwise = ( Nothing
                    , Just pre'
                    , delta m e c)
    step (Nothing, val, e) c = ( Nothing
                               , val
                               , delta m e c)

    combine (_, val, _) = reverse <$> val

-- product construction

dfaProduct :: DFA a -> DFA b -> ((a,b) -> Bool) -> DFA (a, b)
dfaProduct m1 m2 fin
  = DFA {
      start = (start m1, start m2)
    , delta = delta'
    , finals = fin
    }
    where
      delta' (e1,e2) c = (delta m1 e1 c, delta m2 e2 c) 

-- union / intersection

unionDFA :: DFA a -> DFA b -> DFA (a,b)
unionDFA m1 m2 = dfaProduct m1 m2 g
  where
    g (e1, e2) = finals m1 e1 || finals m2 e2

intersectionDFA :: DFA a -> DFA b -> DFA (a,b)
intersectionDFA m1 m2 = dfaProduct m1 m2 g
  where
    g (e1, e2) = finals m1 e1 && finals m2 e2
