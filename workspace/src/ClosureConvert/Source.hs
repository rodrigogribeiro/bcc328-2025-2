module ClosureConvert.Source where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import Data.List (intercalate, nub, intersect)

-- Source language: Lambda calculus with let-bindings
data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  | Let [(String, Expr)] Expr  -- Multiple bindings
  | IntLit Int                 -- Integer literals
  | BoolLit Bool               -- Boolean literals
  | Add Expr Expr              -- Addition operation
  | If Expr Expr Expr          -- Conditional: if cond then e1 else e2
  deriving (Show, Eq)

-- free variables in an expression

freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Lam x body) = filter (/= x) (freeVars body)
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Let bindings body) =
  let bindVars = map fst bindings
      bindExprs = map snd bindings
      exprFvs = nub (concatMap freeVars bindExprs)
      bodyFvs = filter (`notElem` bindVars) (freeVars body)
  in nub (exprFvs ++ bodyFvs)
freeVars (IntLit _) = []
freeVars (BoolLit _) = []
freeVars (Add e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (If cond e1 e2) = nub (freeVars cond ++ freeVars e1 ++ freeVars e2)

-- Dependency analysis for let bindings
-- Build a dependency graph and topologically sort bindings
analyzeDependencies :: [(String, Expr)] -> Either String [(String, Expr)]
analyzeDependencies bindings =
  let names = map fst bindings
      -- Build edges: (from, to) means 'from' depends on 'to'
      edges = [(name, dep) | (name, expr) <- bindings,
                              dep <- (freeVars expr) `intersect` names]
      -- Build adjacency map using algebraic-graphs
      graph = AM.edges edges
      -- Add vertices for bindings with no dependencies
      graphWithVerts = AM.overlay graph (AM.vertices names)
      -- Topological sort using AdjacencyMap.Algorithm
  in case AMA.topSort graphWithVerts of
      Left _ -> Left "Circular dependency detected"
      Right sorted ->
          if AMA.isAcyclic graphWithVerts then
            Right [(name, bind) | (name, (Just bind)) <- map (\ n -> (n, lookup n bindings)) sorted]
          else Left "Circular dependency detected in let bindings"

-- Reorder let bindings according to dependencies before closure conversion
reorderLetBindings :: Expr -> Either String Expr
reorderLetBindings (Let bindings body) = do
  sorted <- analyzeDependencies bindings
  body' <- reorderLetBindings body
  return $ Let sorted body'
reorderLetBindings (Lam param body) = do
  body' <- reorderLetBindings body
  return $ Lam param body'
reorderLetBindings (App e1 e2) = do
  e1' <- reorderLetBindings e1
  e2' <- reorderLetBindings e2
  return $ App e1' e2'
reorderLetBindings (Add e1 e2) = do
  e1' <- reorderLetBindings e1
  e2' <- reorderLetBindings e2
  return $ Add e1' e2'
reorderLetBindings (If cond e1 e2) = do
  cond' <- reorderLetBindings cond
  e1' <- reorderLetBindings e1
  e2' <- reorderLetBindings e2
  return $ If cond' e1' e2'
reorderLetBindings expr = Right expr

-- Pretty printing source language
prettyExpr :: Expr -> String
prettyExpr (Var x) = x
prettyExpr (IntLit n) = show n
prettyExpr (BoolLit True) = "true"
prettyExpr (BoolLit False) = "false"
prettyExpr (Lam param body)
  = "λ" ++ param ++ ". " ++ prettyExpr body
prettyExpr (App e1 e2@(App _ _))
  = prettyExpr e1 ++ " (" ++ prettyExpr e2 ++ ")"
prettyExpr (App e1 e2@(Lam _ _))
  = prettyExpr e1 ++ " (" ++ prettyExpr e2 ++ ")"
prettyExpr (App e1 e2)
  = prettyExpr e1 ++ " " ++ prettyExpr e2
prettyExpr (Add e1 e2)
  = "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
prettyExpr (Let bindings body) =
  "let " ++ intercalate ", " (map (\(x, e) -> x ++ " = " ++ prettyExpr e) bindings) ++
  " in " ++ prettyExpr body
prettyExpr (If cond e1 e2) =
  "if " ++ prettyExpr cond ++ " then " ++ prettyExpr e1 ++ " else " ++ prettyExpr e2
