module Line.Backend.WASM.Pretty where 

import Prelude hiding ((<>))

import Line.Backend.WASM.Syntax
import Utils.Pretty hiding (isEmpty)

instance Pretty WasmType where 
  ppr I32 = text "i32"

instance Pretty WasmInstr where 
  ppr (I32Const n) = text "i32.const" <+> int n
  ppr (LocalGet idx) = text "local.get" <+> int idx
  ppr (LocalSet idx) = text "local.set" <+> int idx
  ppr I32Add = text "i32.add"
  ppr I32Mul = text "i32.mul"
  ppr (Call name) = text "call" <+> text name

prettyFuncSig :: [WasmType] -> [WasmType] -> Doc
prettyFuncSig params results 
  = paramDoc <> resultDoc
  where 
    paramDoc = listCase empty (\ ps' -> parens (text "param" <+> hsep (map ppr ps'))) params 
    resultDoc = listCase empty (\ rs' -> parens (text "result" <+> hsep (map ppr rs'))) results

instance Pretty WasmImport where 
  ppr (WasmImport mod' name (ImportFunc funcName' params results)) 
    = parens $ hsep [ text "import"
                    , doubleQuotes (text mod')
                    , doubleQuotes (text name)
                    , parens (hsep [text "func", text funcName'] <> 
                              prettyFuncSig params results)
                    ]

instance Pretty WasmExport where 
  ppr (WasmExport name (ExportFunc funcName')) 
    = parens $ hsep [ text "export" 
                    , doubleQuotes (text name)
                    , parens (hsep [text "func", text funcName'])
                    ]

instance Pretty WasmFunc where 
  ppr (WasmFunc name params results locals body) 
    = parens (header <> localsDoc <> bodyDoc)
      where 
        header = hsep [text "func", text name] <> prettyFuncSig params results
        localsDoc = listCase empty (\ ls' -> nest 2 (parens (text "local" <+> hsep (map ppr ls')))) locals
        bodyDoc = listCase empty (\ instrs -> nest 2 (vcat (map ppr instrs))) body

instance Pretty WasmModule where 
  ppr (WasmModule imports exports funcs)
    = parens (header <> body)
      where 
        isEmpty = null . render 
        header = text "module"
        sections = filter (not . isEmpty)
                          [
                            listCase empty (vcat . map ppr) imports 
                          , listCase empty (vcat . map ppr) exports 
                          , listCase empty (vcat . map ppr) funcs 
                          ]
        body = listCase empty (nest 2 . vcat) sections 

listCase :: b -> ([a] -> b) -> [a] -> b 
listCase v _ [] = v 
listCase _ f xs = f xs 
