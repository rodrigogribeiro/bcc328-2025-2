module Postfix.PostfixSyntax where

data Exp
  = Const Int
  | Add Exp Exp
  | Mul Exp Exp
  deriving (Eq, Show)
