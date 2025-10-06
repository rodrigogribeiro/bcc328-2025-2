module Line.Backend.RiscV.LineCodegen where

import Control.Monad
import Control.Monad.State 

import Data.Map (Map)
import qualified Data.Map as Map 

import Line.Backend.RiscV.Syntax hiding (instructions, directives)
import Line.Backend.RiscV.Pretty ()
import Line.Frontend.Syntax.LineSyntax

import Utils.Pretty

-- Program code generation

compileLine :: Line -> String 
compileLine = pretty . lineCodegen

lineCodegen :: Line -> Program
lineCodegen ln
  = let    
      s = execState (lineCodegen' ln) initConf
    in Program (reverse $ directives s)
               (reverse $ instructions s)

lineCodegen' :: Line -> CodeGenM () 
lineCodegen' (Line stmts) 
  = do
      -- Function prologue
      emitInstr (Label "main")
      -- Set up stack frame if we have variables
      when (not $ null stmts) $ do
        emitInstr (Addi Sp Sp (-128))  -- allocate stack space
      
      -- Generate code for statements
      mapM_ stmtCodegen stmts
      
      -- Function epilogue  
      when (not $ null stmts) $ do
        emitInstr (Addi Sp Sp 128)     -- restore stack pointer
      emitInstr (Li A0 0)              -- return 0
      emitInstr (Call "exit_program")  -- call exit_program instead of ret

data Conf
  = Conf {
      varMap :: Map Var Int       -- variable name -> stack offset
    , stackOffset :: Int          -- current stack offset
    , tempRegCount :: Int         -- for temporary register allocation
    , instructions :: [Instr]     -- generated instructions
    , directives :: [Directive]   -- assembler directives
    } deriving (Show)

type CodeGenM a = State Conf a

tempRegs :: [Reg]
tempRegs = [T0, T1, T2, T3, T4, T5, T6]


initConf :: Conf  
initConf = Conf Map.empty 0 0 [] [Text, Global "main"]

addVar :: Var -> CodeGenM Int
addVar var = do
  st <- get
  case Map.lookup var (varMap st) of
    Just offset -> return offset
    Nothing -> do
      let newOffset = stackOffset st + 4  -- 4 bytes for 32-bit int
      put st { varMap = Map.insert var newOffset (varMap st)
             , stackOffset = newOffset
             }
      return newOffset

getVarOffset :: Var -> CodeGenM Int
getVarOffset var = do
  st <- get
  case Map.lookup var (varMap st) of
    Just offset -> return offset
    Nothing -> error $ "Undefined variable: " ++ var

emitInstr :: Instr -> CodeGenM ()
emitInstr instr = modify $ \s -> s { instructions = instructions s ++ [instr] }

emitDirective :: Directive -> CodeGenM ()
emitDirective dir = modify $ \s -> s { directives = directives s ++ [dir] }

getNextTempReg :: CodeGenM Reg
getNextTempReg = do
  st <- get
  let regIndex = tempRegCount st `mod` length tempRegs
      reg = tempRegs !! regIndex
  put st { tempRegCount = tempRegCount st + 1 }
  return reg

expCodegen :: Exp -> CodeGenM Reg
expCodegen (EInt n) = do
  reg <- getNextTempReg
  emitInstr (Li reg n)
  return reg
expCodegen (EVar var) = do
  offset <- getVarOffset var
  reg <- getNextTempReg
  emitInstr (Lw reg (-offset) Sp)  -- negative offset from stack pointer
  return reg
expCodegen (e1 :+: e2) = do
  reg1 <- expCodegen e1
  reg2 <- expCodegen e2
  resultReg <- getNextTempReg
  emitInstr (Add resultReg reg1 reg2)
  return resultReg
expCodegen (e1 :*: e2) = do
  reg1 <- expCodegen e1
  reg2 <- expCodegen e2
  resultReg <- getNextTempReg
  emitInstr (Mul resultReg reg1 reg2)
  return resultReg


stmtCodegen :: Stmt -> CodeGenM ()
stmtCodegen (SAssign var ex) = do
  resultReg <- expCodegen ex
  offset <- addVar var
  emitInstr (Sw resultReg (-offset) Sp)  -- store to stack
stmtCodegen (SRead var) = do
  -- Allocate space for the integer on stack
  offset <- addVar var
  -- Load address of variable into A0 (first argument)
  emitInstr (Addi A0 Sp (-offset))
  -- Call read_int function with pointer to variable
  emitInstr (Call "read_int")
stmtCodegen (SPrint ex) = do
  resultReg <- expCodegen ex
  emitInstr (Mv A0 resultReg)  -- move result to argument register
  emitInstr (Call "print_int")


