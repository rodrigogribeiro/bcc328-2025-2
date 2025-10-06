module Exp.Interp.ExpInterp (eval) where

import Exp.Frontend.Syntax.ExpSyntax

-- top level evalreter function

eval :: Exp -> Int
eval (EInt n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
