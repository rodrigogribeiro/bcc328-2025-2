{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ClosureConvert.CC where

import Control.Monad.State
import ClosureConvert.Source
import ClosureConvert.Target

-- Closure conversion with lambda lifting
closureConvert :: Expr -> Program
closureConvert expr = runGen $ do
  (cexpr, funs) <- convert expr []
  return $ Program funs cexpr
  where
    convert :: Expr -> [String] -> Gen (CExpr, [FunDef])
    convert (Var x) _ = return (CVar x, [])

    convert (Lam param body) boundVars = do
      fname <- freshName "lambda_"
      -- Capture all free variables of this lambda
      let fvs = freeVars (Lam param body)
      (body', bodyFuns) <- convert body (param : boundVars)
      let funDef = FunDef fname fvs param body'
      return (CClosure fname (map CVar fvs), funDef : bodyFuns)

    convert (App e1 e2) boundVars = do
      (e1', funs1) <- convert e1 boundVars
      (e2', funs2) <- convert e2 boundVars
      return (CApp e1' e2', funs1 ++ funs2)

    convert (Let bindings body) boundVars = do
      let bindVars = map fst bindings
      -- Convert all binding expressions
      convertedBindings <- mapM (\(x, e) -> do
        (e', funs) <- convert e boundVars
        return ((x, e'), funs)) bindings
      let bindings' = map fst convertedBindings
          bindFuns = concatMap snd convertedBindings
      -- Convert body with all bound variables in scope
      (body', bodyFuns) <- convert body (bindVars ++ boundVars)
      return (CLet bindings' body', bindFuns ++ bodyFuns)

    convert (IntLit n) _ = return (CIntLit n, [])

    convert (BoolLit b) _ = return (CBoolLit b, [])

    convert (Add e1 e2) boundVars = do
      (e1', funs1) <- convert e1 boundVars
      (e2', funs2) <- convert e2 boundVars
      return (CAdd e1' e2', funs1 ++ funs2)

    convert (If cond e1 e2) boundVars = do
      (cond', funsCond) <- convert cond boundVars
      (e1', funs1) <- convert e1 boundVars
      (e2', funs2) <- convert e2 boundVars
      return (CIf cond' e1' e2', funsCond ++ funs1 ++ funs2)


-- Monad for generating fresh names
newtype Gen a = Gen { unGen :: State Int a }
  deriving (Functor, Applicative, Monad, MonadState Int)

runGen :: Gen a -> a
runGen m = evalState (unGen m) 0

freshName :: String -> Gen String
freshName prefix = do
  n <- get
  put (n + 1)
  return $ prefix ++ show n
