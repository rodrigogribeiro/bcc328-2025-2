module MiniML.Frontend.TypeInference.SolverMonad where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as Map
import MiniML.Frontend.Syntax.Type

data Env
  = Env {
      subst :: Subst
    , ctx   :: Ctx
    , counter :: Int
    , infered :: [(Name, Scheme)]
    , logs :: [String]
    }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty 0 [] []

type Solve a = (ExceptT String (StateT Env Identity)) a

runSolve :: Solve a -> Either String (Env, Type)
runSolve m
  = case runIdentity (runStateT (runExceptT m) emptyEnv) of
      (Left err, _) -> Left err
      (Right _, env) ->
        let s = subst env
            v = TVar "v0"
            t = apply s v
        in Right (env, t)


withLocalCtx :: Name -> Scheme -> Solve a -> Solve a
withLocalCtx n sch m
  = do
      env <- gets ctx
      modify (\ s -> s{ctx = Map.insert n sch env})
      r <- m
      modify (\ s -> s{ctx = env})
      pure r

askEnv :: Solve Ctx
askEnv = gets ctx

askSubst :: Solve Subst
askSubst = gets subst

extSubst :: Subst -> Solve ()
extSubst s
  = modify (\env -> env {subst = s @@ (subst env)})

addInferedType :: Name -> Scheme -> Solve ()
addInferedType n sch
  = modify (\ env -> env {infered = (n, sch) : (infered env)})

info :: [String] -> Solve ()
info s = modify (\ env -> env {logs = (unwords s) : logs env})

freshTyVar :: Solve Type
freshTyVar
  = do
      n <- inc
      let v = "v" ++ show n
      pure (TVar v)

inc :: Solve Int
inc = do
        n <- gets counter
        modify (\ env -> env {counter = (n + 1) })
        pure n
