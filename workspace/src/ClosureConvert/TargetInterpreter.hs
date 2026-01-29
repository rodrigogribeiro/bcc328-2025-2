module ClosureConvert.TargetInterpreter where

import Control.Monad
import ClosureConvert.Target

-- Interpreter for target language (closure-converted)
data CValue
  = CVInt Int
  | CVBool Bool
  | CVClosure String [CValue]  -- function name, captured values
  deriving (Show, Eq)

type CFunEnv = [(String, FunDef)]

interpretProgram :: Program -> Either String CValue
interpretProgram (Program funs main) =
  let funEnv = map (\f -> (funName f, f)) funs
  in evalC main [] funEnv

evalC :: CExpr -> [(String, CValue)] -> CFunEnv -> Either String CValue
evalC (CVar x) env _ =
  case lookup x env of
    Just v -> Right v
    Nothing -> Left $ "Variable not found: " ++ x
evalC (CIntLit n) _ _ = Right (CVInt n)
evalC (CBoolLit b) _ _ = Right (CVBool b)
evalC (CAdd e1 e2) env funEnv = do
  v1 <- evalC e1 env funEnv
  v2 <- evalC e2 env funEnv
  case (v1, v2) of
    (CVInt n1, CVInt n2) -> Right (CVInt (n1 + n2))
    _ -> Left "Addition requires two integers"
evalC (CIf cond e1 e2) env funEnv = do
  v <- evalC cond env funEnv
  case v of
    CVBool True -> evalC e1 env funEnv
    CVBool False -> evalC e2 env funEnv
    _ -> Left "Condition must be a boolean"
evalC (CLet bindings body) env funEnv = do
  -- Evaluate all bindings and extend environment
  let evalBinding env' (x, e) = do
        v <- evalC e env funEnv
        return ((x, v) : env')
  env' <- foldM evalBinding env bindings
  evalC body env' funEnv
evalC (CClosure fname capturedExprs) env funEnv = do
  capturedVals <- mapM (\e -> evalC e env funEnv) capturedExprs
  Right (CVClosure fname capturedVals)
evalC (CApp e1 e2) env funEnv = do
  v1 <- evalC e1 env funEnv
  v2 <- evalC e2 env funEnv
  case v1 of
    CVClosure fname capturedVals ->
      case lookup fname funEnv of
        Just (FunDef _ fvNames param body) ->
          let fvEnv = zip fvNames capturedVals
              newEnv = (param, v2) : fvEnv
          in evalC body newEnv funEnv
        Nothing -> Left $ "Function not found: " ++ fname
    _ -> Left "Trying to apply a non-function"

prettyCValue :: CValue -> String
prettyCValue (CVInt n) = show n
prettyCValue (CVBool True) = "true"
prettyCValue (CVBool False) = "false"
prettyCValue (CVClosure fname _) = "<closure " ++ fname ++ ">"
