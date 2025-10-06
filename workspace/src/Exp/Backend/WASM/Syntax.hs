module Exp.Backend.WASM.Syntax where

-- WebAssembly AST abstractions
data WasmType = I32
  deriving (Show, Eq)

data WasmInstr
  = I32Const Int
  | I32Add
  | I32Mul
  | Call String
  | Drop
  deriving (Show, Eq)

data WasmFunc
  = WasmFunc {
      funcName :: String
    , funcParams :: [(String, WasmType)]
    , funcResult :: Maybe WasmType
    , funcLocals :: [(String, WasmType)]
    , funcBody :: [WasmInstr]
    } deriving (Show, Eq)

data WasmImport
  = WasmImport {
      importModule :: String
    , importName :: String
    , importFunc :: String
    , importType :: ([WasmType], Maybe WasmType)
    }
  | WasmMemory String Int
  deriving (Show, Eq)

data WasmModule
  = WasmModule {
      moduleImports :: [WasmImport]
    , moduleFunctions :: [WasmFunc]
    , moduleExports :: [(String, String)]
    , moduleStart :: Maybe String
  } deriving (Show, Eq)
