module ClosureConvert.SourceInterpreter where

import Control.Monad
import ClosureConvert.Source

-- Interpreter for source language
type Env = [(String, Value)]

data Value
  = VInt Int
  | VBool Bool
  | VClosure String Expr Env  -- parameter, body, captured environment
  deriving (Show, Eq)

interpret :: Expr -> Either String Value
interpret expr = eval expr []

eval :: Expr -> Env -> Either String Value
eval (Var x) env =
  case lookup x env of
    Just v -> Right v
    Nothing -> Left $ "Variable not found: " ++ x
eval (IntLit n) _ = Right (VInt n)
eval (BoolLit b) _ = Right (VBool b)
eval (Lam param body) env = Right (VClosure param body env)
eval (App e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case v1 of
    VClosure param body closureEnv ->
      eval body ((param, v2) : closureEnv)
    _ -> Left "Trying to apply a non-function"
eval (Add e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right (VInt (n1 + n2))
    _ -> Left "Addition requires two integers"
eval (If cond e1 e2) env = do
  v <- eval cond env
  case v of
    VBool True -> eval e1 env
    VBool False -> eval e2 env
    _ -> Left "Condition must be a boolean"
eval (Let bindings body) env = do
  -- Evaluate all bindings and extend environment
  let evalBinding env' (x, e) = do
        v <- eval e env
        return ((x, v) : env')
  env' <- foldM evalBinding env bindings
  eval body env'

prettyValue :: Value -> String
prettyValue (VInt n) = show n
prettyValue (VBool True) = "true"
prettyValue (VBool False) = "false"
prettyValue (VClosure param _ _) = "<closure λ" ++ param ++ ".>"
