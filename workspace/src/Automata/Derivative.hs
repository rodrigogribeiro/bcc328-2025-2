module Automata.Derivative ( nullable
                           , (.+.)
                           , (.@.)
                           , star
                           , deriv
                           , match
                           )where

import Automata.RegExp


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
