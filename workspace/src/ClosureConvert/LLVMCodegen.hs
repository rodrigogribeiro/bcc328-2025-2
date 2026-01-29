{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ClosureConvert.LLVMCodegen where

import Control.Monad
import Control.Monad.State
import ClosureConvert.Target

import Data.List

-- LLVM IR code generation
newtype LLVMGen a = LLVMGen { unLLVMGen :: State Int a }
  deriving (Functor, Applicative, Monad, MonadState Int)

runLLVMGen :: LLVMGen a -> a
runLLVMGen m = evalState (unLLVMGen m) 0

freshReg :: LLVMGen String
freshReg = do
  n <- get
  put (n + 1)
  return $ "%r" ++ show n

freshLabel :: String -> LLVMGen String
freshLabel prefix = do
  n <- get
  put (n + 1)
  return $ prefix ++ show n

generateLLVM :: Program -> String
generateLLVM (Program funs main) = runLLVMGen $ do
  -- Generate function definitions
  funDefs <- mapM generateLLVMFunDef funs
  -- Generate main function
  mainCode <- generateLLVMMain main
  return $ unlines
    [ "; Generated LLVM IR"
    , "declare i32 @printf(i8*, ...)"
    , ""
    , "@.str = private unnamed_addr constant [13 x i8] c\"Result: %d\\0A\\00\", align 1"
    , ""
    ] ++ unlines funDefs ++ "\n" ++ mainCode

generateLLVMFunDef :: FunDef -> LLVMGen String
generateLLVMFunDef (FunDef name fvs param body) = do
  let allParams = fvs ++ [param]
      paramDecls = intercalate ", " (map (\p -> "i32 %" ++ p) allParams)
  -- Create initial environment mapping parameters to their names
  let initEnv = [(p, "%" ++ p) | p <- allParams]
  (resultReg, bodyCode) <- generateLLVMCExpr body initEnv
  return $ unlines
    [ "define i32 @" ++ name ++ "(" ++ paramDecls ++ ") {"
    , bodyCode
    , "  ret i32 " ++ resultReg
    , "}"
    ]

generateLLVMMain :: CExpr -> LLVMGen String
generateLLVMMain expr = do
  (resultReg, bodyCode) <- generateLLVMCExpr expr []
  return $ unlines
    [ "define i32 @main() {"
    , bodyCode
    , "  %fmt = getelementptr [13 x i8], [13 x i8]* @.str, i32 0, i32 0"
    , "  call i32 (i8*, ...) @printf(i8* %fmt, i32 " ++ resultReg ++ ")"
    , "  ret i32 0"
    , "}"
    ]

-- Generate LLVM IR for a CExpr
-- Returns (result register, code)
generateLLVMCExpr :: CExpr -> [(String, String)] -> LLVMGen (String, String)
generateLLVMCExpr (CVar x) env =
  case lookup x env of
    Just reg -> return (reg, "")
    Nothing -> error $ "Variable not found in LLVM generation: " ++ x
generateLLVMCExpr (CIntLit n) _ = return (show n, "")
generateLLVMCExpr (CBoolLit True) _ = return ("1", "")
generateLLVMCExpr (CBoolLit False) _ = return ("0", "")
generateLLVMCExpr (CAdd e1 e2) env = do
  (r1, code1) <- generateLLVMCExpr e1 env
  (r2, code2) <- generateLLVMCExpr e2 env
  result <- freshReg
  let addCode = "  " ++ result ++ " = add i32 " ++ r1 ++ ", " ++ r2
  return (result, code1 ++ code2 ++ addCode ++ "\n")
generateLLVMCExpr (CIf cond e1 e2) env = do
  (condReg, condCode) <- generateLLVMCExpr cond env
  thenLabel <- freshLabel "then"
  elseLabel <- freshLabel "else"
  mergeLabel <- freshLabel "merge"
  -- Convert condition to i1
  condBool <- freshReg
  let condConv = "  " ++ condBool ++ " = icmp ne i32 " ++ condReg ++ ", 0\n"
  -- Generate then branch
  (thenReg, thenCode) <- generateLLVMCExpr e1 env
  -- Generate else branch
  (elseReg, elseCode) <- generateLLVMCExpr e2 env
  result <- freshReg
  let branchCode = unlines
        [ concat [ condCode
                 , condConv
                 , "  br i1 "
                 , condBool
                 , ", label %"
                 , thenLabel
                 , ", label %"
                 , elseLabel
                 ]
        , thenLabel ++ ":"
        , thenCode ++ "  br label %" ++ mergeLabel
        , elseLabel ++ ":"
        , elseCode ++ "  br label %" ++ mergeLabel
        , mergeLabel ++ ":"
        , concat [ "  " ++ result
                 , " = phi i32 [ "
                 , thenReg ++ ", %"
                 , thenLabel ++ " ], [ "
                 , elseReg ++ ", %"
                 , elseLabel ++ " ]"
                 ]
        ]
  return (result, branchCode)
generateLLVMCExpr (CLet bindings body) env = do
  -- Process bindings sequentially, accumulating environment
  (finalEnv, bindingsCode) <- foldM procesBinding (env, "") bindings
  -- Generate body with extended environment
  (resultReg, bodyCode) <- generateLLVMCExpr body finalEnv
  return (resultReg, bindingsCode ++ bodyCode)
  where
    procesBinding (currentEnv, accCode) (name, expr) = do
      (reg, code) <- generateLLVMCExpr expr currentEnv
      return ((name, reg) : currentEnv, accCode ++ code)
generateLLVMCExpr (CClosure _ _) _ = do
  -- For now, we can't create first-class closures in our simple LLVM
  -- This would require heap allocation and function pointers
  return ("0", "; Warning: closure creation not fully implemented\n")
generateLLVMCExpr (CApp (CVar fname) arg) env = do
  (argReg, argCode) <- generateLLVMCExpr arg env
  result <- freshReg
  let callCode = "  " ++ result ++ " = call i32 @" ++ fname ++ "(i32 " ++ argReg ++ ")\n"
  return (result, argCode ++ callCode)
generateLLVMCExpr (CApp (CClosure fname capturedExprs) arg) env = do
  -- Evaluate captured variables
  capturedRegs <- mapM (\e -> generateLLVMCExpr e env) capturedExprs
  let (capRegs, capCodes) = unzip capturedRegs
  -- Evaluate argument
  (argReg, argCode) <- generateLLVMCExpr arg env
  -- Call function with captured values and argument
  result <- freshReg
  let allArgs = intercalate ", " (map (\r -> "i32 " ++ r) (capRegs ++ [argReg]))
  let callCode = "  " ++ result ++ " = call i32 @" ++ fname ++ "(" ++ allArgs ++ ")\n"
  return (result, concat capCodes ++ argCode ++ callCode)
generateLLVMCExpr (CApp _ _) _ = do
  -- Complex applications not supported in simple LLVM generation
  return ("0", "; Warning: complex application not implemented\n")
