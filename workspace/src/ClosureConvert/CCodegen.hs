module ClosureConvert.CCodegen where

import Data.List
import ClosureConvert.Target

-- C code generation
generateC :: Program -> String
generateC (Program funs main) =
  unlines
    [ "#include <stdio.h>"
    , "#include <stdlib.h>"
    , ""
    , "// Closure representation"
    , "typedef struct {"
    , "    void* func_ptr;"
    , "    int* env;"
    , "    int env_size;"
    , "} Closure;"
    , ""
    , "// Forward declarations"
    , unlines (map funcForwardDecl funs)
    , unlines (map generateFunDef funs)
    , "int main() {\n"
    , "    int result = " ++ generateCExpr main ++ ";"
    , "    printf(\"Result: %d\\n\", result);"
    , "    return 0;"
    , "}"
    ]

funcForwardDecl :: FunDef -> String
funcForwardDecl (FunDef name fvs param _) =
  let allParams = map (\v -> "int " ++ v) (fvs ++ [param])
  in "int " ++ name ++ "(" ++ intercalate ", " allParams ++ ");"

generateFunDef :: FunDef -> String
generateFunDef (FunDef name fvs param body) =
  let allParams = map (\v -> "int " ++ v) (fvs ++ [param])
      funSig = "int " ++ name ++ "(" ++ intercalate ", " allParams ++ ")"
  in funSig ++ " {\n" ++
     "    return " ++ generateCExpr body ++ ";\n" ++
     "}"

generateCExpr :: CExpr -> String
generateCExpr (CVar x) = x
generateCExpr (CIntLit n) = show n
generateCExpr (CBoolLit True) = "1"
generateCExpr (CBoolLit False) = "0"
generateCExpr (CAdd e1 e2) = "(" ++ generateCExpr e1 ++ " + " ++ generateCExpr e2 ++ ")"
generateCExpr (CIf cond e1 e2) =
  "(" ++ generateCExpr cond ++ " ? " ++ generateCExpr e1 ++ " : " ++ generateCExpr e2 ++ ")"
generateCExpr (CLet bindings body) =
  "({ " ++
  concatMap (\(x, e) -> "int " ++ x ++ " = " ++ generateCExpr e ++ "; ") bindings ++
  generateCExpr body ++ "; })"
generateCExpr (CApp (CVar fname) arg) =
  fname ++ "(" ++ generateCExpr arg ++ ")"
generateCExpr (CApp (CClosure fname capturedVars) arg) =
  fname ++ "(" ++ intercalate ", " (map generateCExpr capturedVars ++ [generateCExpr arg]) ++ ")"
generateCExpr (CClosure fname capturedVars) =
  "/* closure: " ++ fname ++ " with " ++ show (length capturedVars) ++ " captured vars */"
generateCExpr (CApp e1 e2) =
  "/* complex application: (" ++ generateCExpr e1 ++ ")(" ++ generateCExpr e2 ++ ") */"
