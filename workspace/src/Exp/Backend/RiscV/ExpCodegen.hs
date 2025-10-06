module Exp.Backend.RiscV.ExpCodegen where

import Control.Monad.State

import Exp.Backend.RiscV.Syntax
import Exp.Backend.RiscV.Pretty ()
import Exp.Frontend.Syntax.ExpSyntax
import Utils.Pretty

-- Code generation configuration
data Conf
  = Conf {
      nextReg    :: Reg      -- Next available temporary register
    , instrs     :: [Instr]  -- Generated instructions (in reverse order)
    }

-- Initialize code generation state
initConf :: Conf
initConf
  = Conf {
      nextReg = X5
    , instrs = []
    }

-- monad definition

type CodeGen = State Conf


-- Emit an instruction
emit :: Instr -> CodeGen ()
emit instr = modify $ \s -> s { instrs = instr : instrs s }


allocReg :: CodeGen Reg
allocReg
  = do
    s <- get
    let reg = nextReg s
    put s { nextReg = succ reg }
    return reg


-- code generation for expressions

codegenExp :: Exp -> CodeGen Reg
codegenExp (EInt n)
  = do
      reg <- allocReg
      emit (Li reg n)
      return reg
codegenExp (e1 :+: e2)
  = do
      reg1 <- codegenExp e1
      reg2 <- codegenExp e2
      resultReg <- allocReg
      emit (Add resultReg reg1 reg2)
      return resultReg
codegenExp (e1 :*: e2)
  = do
      reg1 <- codegenExp e1
      reg2 <- codegenExp e2
      resultReg <- allocReg
      emit (Mul resultReg reg1 reg2)
      return resultReg

-- Generate complete program with prologue and epilogue
codegenProgram :: Exp -> RiscV
codegenProgram expr
  = let (_, finalState) = runState computation initConf
        computation
          = do
              -- Generate assembly directives and main function
              emit (Directive ".text")
              emit (Directive ".globl main")
              emit (Label "main")

              -- Generate code for the expression
              reg <- codegenExp expr

              -- Move result to a0 (x10) for function call
              emit (Mv X10 reg)

              -- Call print_int function
              emit (Call "print_int")

              -- Set return value to 0
              emit (Li X10 0)
              emit (Call "exit_program")

              return reg
    in RiscV (reverse (instrs finalState))

compileExpression :: Exp -> String
compileExpression = pretty . codegenProgram
