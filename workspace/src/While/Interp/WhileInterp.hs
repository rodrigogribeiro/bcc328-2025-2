module While.Interp.WhileInterp where

import Control.Monad.Except
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Utils.Pretty
import Utils.Value 

import While.Frontend.Pretty.WhilePretty ()
import While.Frontend.Syntax.WhileSyntax

-- main interpreter function

interp :: While -> IO ()
interp p = do
    (r, _) <- runStateT (runExceptT (interpWhile p)) Map.empty
    case r of
      Left err -> putStrLn err
      Right _ -> pure ()

-- interpreting programs

interpWhile :: While -> Interp ()
interpWhile (While ss)
  = mapM_ interpStmt ss

-- evaluating statements

interpStmt :: Stmt -> Interp ()
interpStmt (SAssign v e)
  = do
      val <- interpExp e
      updateVar v val
interpStmt (SRead v)
  = do
      str <- liftIO $ getLine
      let val = read str
      updateVar v (VInt val)
interpStmt (SPrint e)
  = do
      val <-interpExp e
      liftIO $ print val
interpStmt (SIf e blk1 blk2)
  = do 
      v <- interpExp e 
      case v of 
        VBool True-> mapM_ interpStmt blk1 
        VBool False -> mapM_ interpStmt blk2 
        _ -> throwError $ unlines ["Expecting a boolean value, but found:", pretty e]
interpStmt (SWhile e blk1)
  = do 
      v <- interpExp e 
      case v of 
        VBool False -> pure ()
        VBool True -> do 
          mapM_ interpStmt blk1 
          interpStmt (SWhile e blk1)
        _ -> throwError $ unlines ["Expecting a boolean value, but found:", pretty e]

-- evaluating expressions

interpExp :: Exp -> Interp Value 
interpExp (EValue v) = pure v
interpExp (EVar v) = askVar v
interpExp (e1 :+: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .+. v2
interpExp (e1 :*: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .*. v2
interpExp (e1 :<: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .<. v2
interpExp (e1 :=: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .=. v2
interpExp (e1 :|: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .|. v2
interpExp (ENot e)
  = do 
      v <- interpExp e 
      vnot v 


-- monad and environment definition

type Env = Map String Value
type Interp a = ExceptT String (StateT Env IO) a

askVar :: String -> Interp Value
askVar s
  = do
      env <- get
      case Map.lookup s env of
        Nothing -> throwError $ unwords ["Undefined variable:", s]
        Just val -> pure val

updateVar :: String -> Value -> Interp ()
updateVar s val = modify (Map.insert s val)


