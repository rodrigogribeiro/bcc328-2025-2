module Exp.Backend.RiscV.Syntax where

-- RISC-V Register type
data Reg
  = X0 | X1 | X2 | X3 | X4 | X5 | X6
  | X7 | X8 | X9 | X10 | X11 | X12 | X13
  | X14 | X15 | X16 | X17 | X18 | X19 | X20
  | X21 | X22 | X23 | X24 | X25 | X26 | X27
  | X28 | X29 | X30 | X31
  deriving (Eq, Ord, Show, Enum)

data Instr
  = Li Reg Int           -- Load immediate
  | Add Reg Reg Reg      -- Add rd = rs1 + rs2
  | Mul Reg Reg Reg      -- Multiply rd = rs1 * rs2
  | Mv Reg Reg           -- Move rd = rs1 (pseudo-instruction)
  | Call String          -- Function call
  | Ret                  -- Return
  | Label String         -- Label
  | Directive String     -- Assembly directive
  deriving (Eq, Show)

data RiscV
  = RiscV { out :: [Instr] }
    deriving (Eq, Show)
