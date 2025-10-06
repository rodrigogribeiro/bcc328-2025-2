module Line.Interp.LineInterp where

import Control.Monad.Except
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Line.Frontend.Syntax.LineSyntax

-- main interpreter function

interp :: Line -> IO ()
interp p = do
    (r, _) <- runStateT (runExceptT (interpProgram p)) Map.empty
    case r of
      Left err -> putStrLn err
      Right _ -> pure ()

-- interpreting programs

interpProgram :: Line -> Interp ()
interpProgram (Line ss)
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
      updateVar v val
interpStmt (SPrint e)
  = do
      val <-interpExp e
      liftIO $ print val

-- evaluating expressions

interpExp :: Exp -> Interp Int
interpExp (EInt n) = pure n
interpExp (EVar v) = askVar v
interpExp (e1 :+: e2)
  = (+) <$> interpExp e1 <*> interpExp e2
interpExp (e1 :*: e2)
  = (*) <$> interpExp e1 <*> interpExp e2

-- monad and environment definition

type Env = Map String Int
type Interp a = ExceptT String (StateT Env IO) a

askVar :: String -> Interp Int
askVar s
  = do
      env <- get
      case Map.lookup s env of
        Nothing -> throwError $ unwords ["Undefined variable:", s]
        Just val -> pure val

updateVar :: String -> Int -> Interp ()
updateVar s val = modify (Map.insert s val)


