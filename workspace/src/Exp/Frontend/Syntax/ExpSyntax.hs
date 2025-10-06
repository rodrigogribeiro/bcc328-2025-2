module Exp.Frontend.Syntax.ExpSyntax (Exp (..)) where

-- definition of simple expressions

data Exp
  = EInt Int
  | Exp :+: Exp
  | Exp :*: Exp
  deriving (Eq, Ord, Show)
