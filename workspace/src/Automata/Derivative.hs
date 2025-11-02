module Automata.Derivative ( nullable
                           , (.+.)
                           , (.@.)
                           , star
                           , deriv
                           , match
                           , derivDFA
                           )where

import Data.List 
import Automata.RegExp
import Automata.DFA


-- nullability test

nullable :: Regex -> Bool
nullable Empty = False
nullable Lambda = True
nullable (Chr _) = False
nullable (e1 :+: e2)
  = nullable e1 || nullable e2
nullable (e1 :@: e2)
  = nullable e1 && nullable e2
nullable (Star _) = True

-- smart constructions

(.+.) :: Regex -> Regex -> Regex
Empty .+. e' = e'
e .+. Empty  = e
e .+. e'     = e :+: e'

(.@.) :: Regex -> Regex -> Regex
Empty .@. _ = Empty
_ .@. Empty = Empty
Lambda .@. e' = e'
e .@. Lambda = e
e .@. e' = e :@: e'

star :: Regex -> Regex
star Empty = Lambda
star (Star e) = e
star e = Star e

-- simplify a regex using smart constructors 

simp :: Regex -> Regex 
simp (e1 :+: e2) = e1 .+. e2 
simp (e1 :@: e2) = e1 .@. e2 
simp (Star e) = star e
simp e = e 

-- derivative function

deriv :: Char -> Regex -> Regex
deriv _ Empty  = Empty
deriv _ Lambda = Empty
deriv a (Chr b)
  | a == b = Lambda
  | otherwise = Empty
deriv a (e1 :+: e2)
  = deriv a e1 .+. deriv a e2
deriv a (e1 :@: e2)
  | nullable e1 = deriv a e1 .@. e2 .+. deriv a e2
  | otherwise   = deriv a e1 .@. e2
deriv a (Star e1)
  = deriv a e1 .@. (star e1)

-- matching function

match :: Regex -> String -> Bool
match e [] = nullable e
match e (c : cs) = match (deriv c e) cs

-- building a DFA using derivatives 

enumerateDerivs :: Regex -> [Regex]
enumerateDerivs initial = go [initial] [initial]
  where
    go [] acc = acc
    go (state:rest) acc = 
        let chars = symbols initial 
            newStates = nub [deriv c state | c <- chars]
            unseen = filter (`notElem` acc) newStates
        in go (rest ++ unseen) (acc ++ unseen)

derivDFA :: Regex -> DFA Regex
derivDFA e
  = DFA {
      start = initial
    , delta = delta'
    , finals = nullable 
    }
  where
    initial = simp e
    states = enumerateDerivs initial
    
    delta' state char = 
        let e' = deriv char state
        in if e' `elem` states then e' else Empty
    
