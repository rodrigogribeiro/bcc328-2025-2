module While.Frontend.Syntax.WhileSyntax where

import Utils.Value

data While
  = While [Stmt]
    deriving (Eq, Ord, Show)

type Var = String
type Block = [Stmt]

data Stmt
  = SAssign Var Exp
  | SRead Var
  | SPrint Exp
  | SIf Exp Block Block 
  | SWhile Exp Block 
  deriving (Eq, Ord, Show)

data Exp
  = EValue Value 
  | EVar Var 
  | ENot Exp 
  | Exp :<: Exp 
  | Exp :=: Exp
  | Exp :|: Exp 
  | Exp :+: Exp
  | Exp :*: Exp
  deriving (Eq, Ord, Show)
