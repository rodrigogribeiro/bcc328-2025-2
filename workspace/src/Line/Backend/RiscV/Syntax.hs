module Line.Backend.RiscV.Syntax where 

data Reg 
  = Zero  -- x0 - hardwired zero
  | Ra    -- x1 - return address
  | Sp    -- x2 - stack pointer  
  | Gp    -- x3 - global pointer
  | Tp    -- x4 - thread pointer
  | T0    -- x5 - temporary
  | T1    -- x6 - temporary
  | T2    -- x7 - temporary
  | S0    -- x8 - saved register / frame pointer
  | S1    -- x9 - saved register
  | A0    -- x10 - function argument / return value
  | A1    -- x11 - function argument / return value
  | A2    -- x12 - function argument
  | A3    -- x13 - function argument
  | A4    -- x14 - function argument
  | A5    -- x15 - function argument
  | A6    -- x16 - function argument
  | A7    -- x17 - function argument
  | S2    -- x18 - saved register
  | S3    -- x19 - saved register
  | S4    -- x20 - saved register
  | S5    -- x21 - saved register
  | S6    -- x22 - saved register
  | S7    -- x23 - saved register
  | S8    -- x24 - saved register
  | S9    -- x25 - saved register
  | S10   -- x26 - saved register
  | S11   -- x27 - saved register
  | T3    -- x28 - temporary
  | T4    -- x29 - temporary
  | T5    -- x30 - temporary
  | T6    -- x31 - temporary
  deriving (Eq, Ord, Show)


data Instr
  = Li Reg Int         -- load immediate
  | Lw Reg Int Reg     -- load word: lw rd, offset(rs1)
  | Sw Reg Int Reg     -- store word: sw rs2, offset(rs1)
  | Add Reg Reg Reg    -- add rd, rs1, rs2
  | Addi Reg Reg Int   -- add immediate: addi rd, rs1, imm
  | Sub Reg Reg Reg    -- subtract: sub rd, rs1, rs2
  | Mul Reg Reg Reg    -- multiply: mul rd, rs1, rs2
  | Mv Reg Reg         -- move (pseudo-instruction): mv rd, rs
  | Call String        -- function call
  | Ret                -- return
  | Label String       -- label
  deriving (Eq, Show)

data Directive
  = Global String         -- .global symbol
  | Text                  -- .text section
  | Data                  -- .data section  
  | Word [Int]           -- .word values
  | Space Int            -- .space bytes
  deriving (Eq, Show)


data Program 
  = Program { 
      directives :: [Directive]
    , instructions :: [Instr]
    } deriving (Eq, Show)
