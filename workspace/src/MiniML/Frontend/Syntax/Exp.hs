module MiniML.Frontend.Syntax.Exp where

import MiniML.Frontend.Syntax.Type

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Exp
  = Var Name
  | Lit Lit
  | App Exp Exp
  | Lam Name (Maybe Type) Exp
  | Let Name (Maybe Scheme) Exp Exp
  deriving (Show, Eq, Ord)
