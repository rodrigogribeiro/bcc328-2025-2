module Line.Backend.WASM.Syntax where


data WasmType = I32
  deriving (Eq, Show)

data WasmInstr 
  = I32Const Int
  | LocalGet Int
  | LocalSet Int
  | I32Add
  | I32Mul
  | Call String
  deriving (Eq, Show)

data WasmFunc = WasmFunc
  { funcName :: String
  , funcParams :: [WasmType]
  , funcResults :: [WasmType]
  , funcLocals :: [WasmType]
  , funcBody :: [WasmInstr]
  } deriving (Eq, Show)

data WasmImport = WasmImport
  { importModule :: String
  , importName :: String
  , importDesc :: ImportDesc
  } deriving (Eq, Show)

data ImportDesc = ImportFunc String [WasmType] [WasmType]
  deriving (Eq, Show)

data WasmExport = WasmExport
  { exportName :: String
  , exportDesc :: ExportDesc
  } deriving (Eq, Show)

data ExportDesc = ExportFunc String
  deriving (Eq, Show)

data WasmModule = WasmModule
  { moduleImports :: [WasmImport]
  , moduleExports :: [WasmExport]
  , moduleFuncs :: [WasmFunc]
  } deriving (Eq, Show)

