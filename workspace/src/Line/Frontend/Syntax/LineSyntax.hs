module Line.Frontend.Syntax.LineSyntax where

data Line
  = Line [Stmt]
    deriving (Eq, Ord, Show)

type Var = String

data Stmt
  = SAssign Var Exp
  | SRead Var
  | SPrint Exp
  deriving (Eq, Ord, Show)

data Exp
  = EInt Int
  | EVar Var
  | Exp :+: Exp
  | Exp :*: Exp
  deriving (Eq, Ord, Show)
