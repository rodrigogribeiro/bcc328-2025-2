module Line.Frontend.Pretty.LinePretty where 

import Prelude hiding ((<>))
import Line.Frontend.Syntax.LineSyntax
import Utils.Pretty 

instance Pretty Line where 
  ppr = pprLine 

pprLine :: Line -> Doc 
pprLine (Line stms) 
  = vcat (map pprStmt stms)

pprStmt :: Stmt -> Doc 
pprStmt (SAssign v e)
  = hsep [text v, text ":=", pprExp e, semi]
pprStmt (SRead v)
  = text "read" <> parens (text v) <> semi 
pprStmt (SPrint e)
  = text "print" <> parens (pprExp e) <> semi 

pprExp :: Exp -> Doc 
pprExp = pprAdd 

pprAdd :: Exp -> Doc 
pprAdd (e1 :+: e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd other 
  = pprMul other 

pprMul :: Exp -> Doc 
pprMul (e1 :*: e2)
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul other
  = pprNum other 

pprNum :: Exp -> Doc 
pprNum (EInt n) = int n 
pprNum other = parens (pprExp other)
 
