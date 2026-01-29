module ClosureConvert.Target where

import Data.List

-- Target language after closure conversion
data CExpr
  = CVar String
  | CApp CExpr CExpr
  | CClosure String [CExpr]      -- function name, captured free vars
  | CLet [(String, CExpr)] CExpr -- Multiple bindings
  | CIntLit Int                  -- Integer literals
  | CBoolLit Bool                -- Boolean literals
  | CAdd CExpr CExpr             -- Addition operation
  | CIf CExpr CExpr CExpr        -- Conditional
  deriving (Show, Eq)

-- Top-level function definition
data FunDef = FunDef
  { funName :: String
  , funFreeVars :: [String]      -- captured environment variables
  , funParam :: String           -- the actual parameter
  , funBody :: CExpr
  } deriving (Show, Eq)

-- Complete program with top-level functions and main expression
data Program = Program
  { progFuns :: [FunDef]
  , progMain :: CExpr
  } deriving (Show, Eq)


-- Pretty printing target language
prettyProgram :: Program -> String
prettyProgram (Program funs main) =
  unlines (map prettyFunDef funs) ++ "\nmain =\n  " ++ prettyCExpr main

prettyFunDef :: FunDef -> String
prettyFunDef (FunDef name fvs param body) =
  name ++ "(" ++ unwords (fvs ++ [param]) ++ ") =\n  " ++ prettyCExpr body

prettyCExpr :: CExpr -> String
prettyCExpr (CVar x) = x
prettyCExpr (CApp e1 e2) = "(" ++ prettyCExpr e1 ++ " " ++ prettyCExpr e2 ++ ")"
prettyCExpr (CClosure fname fvs) =
  "<" ++ fname ++ "[" ++ unwords (map prettyCExpr fvs) ++ "]>"
prettyCExpr (CLet bindings body) =
  "let " ++ intercalate ", " (map (\(x, e) -> x ++ " = " ++ prettyCExpr e) bindings) ++
  " in " ++ prettyCExpr body
prettyCExpr (CIntLit n) = show n
prettyCExpr (CBoolLit True) = "true"
prettyCExpr (CBoolLit False) = "false"
prettyCExpr (CAdd e1 e2) = "(" ++ prettyCExpr e1 ++ " + " ++ prettyCExpr e2 ++ ")"
prettyCExpr (CIf cond e1 e2) =
  "if " ++ prettyCExpr cond ++ " then " ++ prettyCExpr e1 ++ " else " ++ prettyCExpr e2


