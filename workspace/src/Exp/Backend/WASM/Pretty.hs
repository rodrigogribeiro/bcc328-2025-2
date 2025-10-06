{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Exp.Backend.WASM.Pretty where

import Exp.Backend.WASM.Syntax
import Utils.Pretty

-- Pretty printing functions using the pretty package

instance Pretty WasmType where
  ppr I32 = text "i32"

instance Pretty WasmInstr where
  ppr (I32Const n) = text "i32.const" <+> int n
  ppr I32Add = text "i32.add"
  ppr I32Mul = text "i32.mul"
  ppr (Call name) = text "call" <+> text ("$" ++ name)
  ppr Drop = text "drop"

instance Pretty [(String, WasmType)] where
  ppr [] = empty
  ppr params = hsep $ map go params
    where
      go (name, typ) = parens (text "param" <+>
                               text ("$" ++ name) <+>
                               ppr typ)

instance Pretty (Maybe WasmType) where
  ppr Nothing = empty
  ppr (Just typ) = parens (text "result" <+> ppr typ)

instance Pretty WasmFunc where
  ppr (WasmFunc name params result locals body)
    = parens $ vcat
        [ text "func" <+> text ("$" ++ name)
        , nest 3 $ ppr params
        , nest 3 $ ppr result
        , nest 3 $ ppr locals
        , nest 3 $ vcat (map ppr body)
        ]

instance Pretty ([WasmType], Maybe WasmType) where
  ppr (params, result)
    = parens $ hsep (map go params) <+> res
    where
      go t = (text "param" <+> ppr t)
      res = case result of
              Nothing -> empty
              Just r -> parens (text "result" <+> ppr r)

instance Pretty WasmImport where
  ppr (WasmImport m name func typ)
    = parens $ text "import" <+>
               doubleQuotes (text m) <+>
               doubleQuotes (text name) <+>
               parens (text "func" <+> text ("$" ++ func) <+> ppr typ)
  ppr (WasmMemory m n)
    = parens $ text "import" <+>
               doubleQuotes (text m) <+>
               doubleQuotes (text "memory") <+>
               parens (text "memory" <+> int n)

prettyExport :: (String, String) -> Doc
prettyExport (name, func)
  = parens $ text "export" <+>
      doubleQuotes (text name) <+>
      parens (text "func" <+> text ("$" ++ func))

prettyStart :: Maybe String -> Doc
prettyStart Nothing = empty
prettyStart (Just func)
  = parens $ text "start" <+> text ("$" ++ func)

instance Pretty WasmModule where
  ppr (WasmModule imports funcs exports start)
    = parens $ text "module" $$
               nest 2 (vcat $ map ppr imports ++
                              map ppr funcs ++
                              map prettyExport exports ++
                              [prettyStart start | start /= Nothing])


