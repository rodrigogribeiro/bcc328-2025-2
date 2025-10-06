module Line.Backend.RiscV.Pretty where 

import Prelude hiding ((<>))
import Line.Backend.RiscV.Syntax 
import Utils.Pretty 

instance Pretty Reg where 
  ppr Zero = text "zero"
  ppr Ra   = text "ra"
  ppr Sp   = text "sp"
  ppr Gp   = text "gp"
  ppr Tp   = text "tp"
  ppr T0   = text "t0"
  ppr T1   = text "t1"
  ppr T2   = text "t2"
  ppr S0   = text "s0"
  ppr S1   = text "s1"
  ppr A0   = text "a0"
  ppr A1   = text "a1"
  ppr A2   = text "a2"
  ppr A3   = text "a3"
  ppr A4   = text "a4"
  ppr A5   = text "a5"
  ppr A6   = text "a6"
  ppr A7   = text "a7"
  ppr S2   = text "s2"
  ppr S3   = text "s3"
  ppr S4   = text "s4"
  ppr S5   = text "s5"
  ppr S6   = text "s6"
  ppr S7   = text "s7"
  ppr S8   = text "s8"
  ppr S9   = text "s9"
  ppr S10  = text "s10"
  ppr S11  = text "s11"
  ppr T3   = text "t3"
  ppr T4   = text "t4"
  ppr T5   = text "t5"
  ppr T6   = text "t6"

instance Pretty Instr where 
  ppr (Li rd imm) = 
    text "li" <+> ppr rd <> comma <+> int imm
  ppr (Lw rd offset rs) =
    text "lw" <+> ppr rd <> comma <+> int offset <> parens (ppr rs)
  ppr (Sw rs offset rd) =
    text "sw" <+> ppr rs <> comma <+> int offset <> parens (ppr rd)
  ppr (Add rd rs1 rs2) =
    text "add" <+> ppr rd <> comma <+> ppr rs1 <> comma <+> ppr rs2
  ppr (Addi rd rs imm) =
    text "addi" <+> ppr rd <> comma <+> ppr rs <> comma <+> int imm
  ppr (Sub rd rs1 rs2) =
    text "sub" <+> ppr rd <> comma <+> ppr rs1 <> comma <+> ppr rs2
  ppr (Mul rd rs1 rs2) =
    text "mul" <+> ppr rd <> comma <+> ppr rs1 <> comma <+> ppr rs2
  ppr (Mv rd rs) =
    text "mv" <+> ppr rd <> comma <+> ppr rs
  ppr (Call name) =
    text "call" <+> text name
  ppr Ret =
    text "ret"
  ppr (Label name) =
    text name <> colon

instance Pretty Directive where 
  ppr (Global name) = text ".global" <+> text name
  ppr Text = text ".text"
  ppr Data = text ".data"  
  ppr (Word values) = text ".word" <+> hsep (punctuate comma (map int values))
  ppr (Space n) = text ".space" <+> int n

instance Pretty Program where 
  ppr (Program dirs ins) 
    = direcs $+$ instrs 
      where 
        direcs = vcat (map ppr dirs)
        instrs = vcat (map ppr ins)
