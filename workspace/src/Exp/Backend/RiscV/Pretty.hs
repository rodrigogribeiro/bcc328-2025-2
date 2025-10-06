module Exp.Backend.RiscV.Pretty where

import Prelude hiding ((<>))
import Exp.Backend.RiscV.Syntax
import Utils.Pretty

instance Pretty Reg where
  ppr X0  = text "x0"
  ppr X1  = text "x1"
  ppr X2  = text "x2"
  ppr X3  = text "x3"
  ppr X4  = text "x4"
  ppr X5  = text "x5"
  ppr X6  = text "x6"
  ppr X7  = text "x7"
  ppr X8  = text "x8"
  ppr X9  = text "x9"
  ppr X10 = text "x10"  -- a0 - function argument/return value
  ppr X11 = text "x11"  -- a1
  ppr X12 = text "x12"  -- a2
  ppr X13 = text "x13"  -- a3
  ppr X14 = text "x14"  -- a4
  ppr X15 = text "x15"  -- a5
  ppr X16 = text "x16"  -- a6
  ppr X17 = text "x17"  -- a7
  ppr X18 = text "x18"  -- s2
  ppr X19 = text "x19"  -- s3
  ppr X20 = text "x20"  -- s4
  ppr X21 = text "x21"  -- s5
  ppr X22 = text "x22"  -- s6
  ppr X23 = text "x23"  -- s7
  ppr X24 = text "x24"  -- s8
  ppr X25 = text "x25"  -- s9
  ppr X26 = text "x26"  -- s10
  ppr X27 = text "x27"  -- s11
  ppr X28 = text "x28"  -- t3
  ppr X29 = text "x29"  -- t4
  ppr X30 = text "x30"  -- t5
  ppr X31 = text "x31"  -- t6

instance Pretty Instr where
  ppr (Li rd imm)
    = text "li" <+> ppr rd <> comma <+> int imm
  ppr (Add rd rs1 rs2)
    = text "add" <+> ppr rd <> comma <+> ppr rs1 <> comma <+> ppr rs2
  ppr (Mul rd rs1 rs2)
    = text "mul" <+> ppr rd <> comma <+> ppr rs1 <> comma <+> ppr rs2
  ppr (Mv rd rs)
    = text "mv" <+> ppr rd <> comma <+> ppr rs
  ppr (Call fn)
    = text "call" <+> text fn
  ppr Ret
    = text "ret"
  ppr (Label lbl)
    = text lbl <> colon
  ppr (Directive dir)
    = text dir

instance Pretty RiscV where
  ppr = vcat . map ppr . out
