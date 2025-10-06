module Exp.Backend.WASM.ExpCodegen where

import Exp.Backend.WASM.Syntax
import Exp.Backend.WASM.Pretty ()
import Exp.Frontend.Syntax.ExpSyntax
import Utils.Pretty


-- compiling expressions into WASM function

compileExp :: Exp -> [WasmInstr]
compileExp (EInt n)
  = [I32Const n]
compileExp (e1 :+: e2)
  = compileExp e1 ++ compileExp e2 ++ [I32Add]
compileExp (e1 :*: e2)
  = compileExp e1 ++ compileExp e2 ++ [I32Mul]

compileToWasm :: Exp -> WasmModule
compileToWasm expr
  = WasmModule {
      moduleImports =
        [ WasmImport "runtime" "print_int" "print_int" ([I32], Nothing)
        , WasmMemory "runtime" 1
        ]
    , moduleFunctions =
        [ WasmFunc "main" [] Nothing [] (compileExp expr ++ [Call "print_int"])
        ]
    , moduleExports =
        [ ("main", "main") ]
    , moduleStart = Just "main"
    }

-- Main compilation function
compileExpression :: Exp -> String
compileExpression expr
  = pretty (compileToWasm expr)
