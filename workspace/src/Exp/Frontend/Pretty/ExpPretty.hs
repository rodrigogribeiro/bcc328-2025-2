module Exp.Frontend.Pretty.ExpPretty where 

import Exp.Frontend.Syntax.ExpSyntax
import Utils.Pretty 

instance Pretty Exp where 
  ppr = pprAdd 

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
pprNum other = parens (ppr other)
  
