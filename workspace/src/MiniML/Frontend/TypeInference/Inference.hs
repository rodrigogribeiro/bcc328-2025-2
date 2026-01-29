module MiniML.Frontend.TypeInference.Inference where

import MiniML.Frontend.Syntax.Exp
import MiniML.Frontend.Syntax.Type
import MiniML.Frontend.TypeInference.Constraint
import MiniML.Frontend.TypeInference.ConstraintGen
import MiniML.Frontend.TypeInference.Solver
import MiniML.Frontend.TypeInference.SolverMonad

-- main type inference driver

infer :: Exp -> Either String (Constraint, Env, Type)
infer e
  = let c = CExists (\ t -> generator e t)
    in case solver c of
         Left err -> Left err
         Right (env, t) -> Right (c, env, t)
