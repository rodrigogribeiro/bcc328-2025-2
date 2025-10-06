module Line.Backend.WASM.LineCodegen where

import Control.Monad.Except 
import Control.Monad.Identity 
import Control.Monad.State 

import Data.Map (Map)
import qualified Data.Map as Map

import Line.Backend.WASM.Syntax
import Line.Backend.WASM.Pretty ()
import Line.Frontend.Syntax.LineSyntax

import Utils.Pretty 

-- Program code generation

compileLine :: Line -> Either String String 
compileLine = either Left (Right . pretty) . lineCodegen 

lineCodegen :: Line -> Either String WasmModule
lineCodegen (Line stmts) 
  = case runCodeGenM (mapM_ stmtCodegen stmts) of 
      Left err -> Left err 
      Right finalState -> 
        let 
          numLocals = nextLocal finalState
          locals = replicate numLocals I32
          mainFunc = WasmFunc { 
                      funcName = "$main"
                    , funcParams = []
                    , funcResults = []
                    , funcLocals = locals
                    , funcBody = instructions finalState
                    }
          imports = [ WasmImport "env" "read_int" (ImportFunc "$read_int" [] [I32])
                    , WasmImport "env" "print_int" (ImportFunc "$print_int" [I32] [])
                    ]
          exports = [ WasmExport "main" (ExportFunc "$main") ]
        in Right $ WasmModule imports exports [mainFunc]

-- Code generation config 

data Conf 
  = Conf { 
      varMap :: Map Var Int
    , nextLocal :: Int
    , instructions :: [WasmInstr]
    } deriving (Show)

type CodeGenM a = StateT Conf (ExceptT String Identity) a 

runCodeGenM :: CodeGenM a -> Either String Conf
runCodeGenM m 
  = runIdentity (runExceptT (execStateT m initConf)) 

-- Initialize code generation config 

initConf :: Conf
initConf = Conf Map.empty 0 []

-- State operations
addVar :: Var -> CodeGenM Int
addVar var = do
  conf <- get
  case Map.lookup var (varMap conf) of
    Just idx -> return idx
    Nothing -> do
      let idx = nextLocal conf
      put conf  { varMap = Map.insert var idx (varMap conf)
                , nextLocal = idx + 1
                }
      return idx

emitInstr :: WasmInstr -> CodeGenM ()
emitInstr instr 
  = modify $ \s -> s { instructions = instructions s ++ [instr] }

getVarIndex :: Var -> CodeGenM Int
getVarIndex var = do
  conf <- get
  case Map.lookup var (varMap conf) of
    Just idx -> return idx
    Nothing -> throwError $ "Undefined variable: " ++ var

-- Expression code generation
expCodegen :: Exp -> CodeGenM ()
expCodegen (EInt n) = emitInstr (I32Const n)
expCodegen (EVar var) = do
  idx <- getVarIndex var
  emitInstr (LocalGet idx)
expCodegen (e1 :+: e2) = do
  expCodegen e1
  expCodegen e2
  emitInstr I32Add
expCodegen (e1 :*: e2) = do
  expCodegen e1
  expCodegen e2
  emitInstr I32Mul

-- Statement code generation
stmtCodegen :: Stmt -> CodeGenM ()
stmtCodegen (SAssign var e) = do
  expCodegen e
  idx <- addVar var
  emitInstr (LocalSet idx)
stmtCodegen (SRead var) = do
  emitInstr (Call "$read_int")
  idx <- addVar var
  emitInstr (LocalSet idx)
stmtCodegen (SPrint e) = do
  expCodegen e
  emitInstr (Call "$print_int")


