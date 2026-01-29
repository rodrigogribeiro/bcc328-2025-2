module MiniML.Frontend.TypeInference.Constraint where

import MiniML.Frontend.Syntax.Type

data Constraint
  = CTrue
  | Type :=: Type
  | Constraint :&: Constraint
  | CExists (Type -> Constraint)
  | CDef Name Type Constraint
  | CLet Name Type Constraint Constraint
  | CInst Name Type
  | CInstScheme Scheme Type



