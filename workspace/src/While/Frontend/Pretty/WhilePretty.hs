module While.Frontend.Pretty.WhilePretty where 

import Prelude hiding ((<>))
import Utils.Pretty 
import While.Frontend.Syntax.WhileSyntax

instance Pretty While where 
  ppr (While stmts) 
    = text "{" $$ 
        nest 3 (vcat (map ppr stmts)) $$
      text "}"

instance Pretty Stmt where
  ppr (SAssign v e)
    = hsep [text v, text ":=", ppr e, semi]
  ppr (SRead v)
    = text "read" <> parens (text v) <> semi 
  ppr (SPrint e)
    = text "print" <> parens (ppr e) <> semi 
  ppr (SIf e blk1 [])
    = text "if" <> parens (ppr e) <> text "{" $$ 
        nest 3 (vcat (map ppr blk1)) $$ 
      text "}"
  ppr (SIf e blk1 blk2)
    = text "if" <> parens (ppr e) <> text "{" $$ 
        nest 3 (vcat (map ppr blk1)) $$ 
      text "}" <> text "else {" $$ 
        nest 3 (vcat (map ppr blk2)) $$ 
      text "}"
  ppr (SWhile e blk)
    = text "while" <> parens (ppr e) <> text "{" $$
        nest 3 (vcat (map ppr blk)) $$ 
      text "}"
    
instance Pretty Exp where
  ppr = pprOr 

pprOr :: Exp -> Doc 
pprOr (e1 :|: e2)
  = hsep [pprOr e1, text "||", pprOr e2]
pprOr e = pprRel e 

pprRel :: Exp -> Doc 
pprRel (e1 :<: e2) 
  = hsep [pprRel e1, text "<", pprRel e2]
pprRel (e1 :=: e2) 
  = hsep [pprRel e1, text "=", pprRel e2]
pprRel e = pprAdd e 

pprAdd :: Exp -> Doc
pprAdd (e1 :+: e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd e = pprMul e 

pprMul :: Exp -> Doc 
pprMul (e1 :*: e2)
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul e = pprNot e 

pprNot :: Exp -> Doc 
pprNot (ENot e) = text "!" <+> pprNot e 
pprNot e = pprFact e 

pprFact :: Exp -> Doc 
pprFact (EValue val) = ppr val 
pprFact (EVar v) = text v 
pprFact e = parens (ppr e)
