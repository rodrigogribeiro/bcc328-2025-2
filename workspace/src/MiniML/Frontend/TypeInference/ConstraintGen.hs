module MiniML.Frontend.TypeInference.ConstraintGen where

import MiniML.Frontend.Syntax.Exp
import MiniML.Frontend.Syntax.Type
import MiniML.Frontend.TypeInference.Constraint

generator :: Exp -> Type -> Constraint
generator (Var n) ty
  = CInst n ty
generator (Lit l) ty
  = ty :=: (typeOfLit l)
generator (App e1 e2) ty
  = CExists (\ ty' ->
      let
        c1 = generator e1 (TFun ty' ty)
        c2 = generator e2 ty'
      in c1 :&: c2)
generator (Lam x mt body) ty
  = CExists (\ t1 ->
      CExists (\ t2 ->
        let
          cBody = generator body t2
          cBase = (CDef x t1 cBody) :&: (ty :=: (TFun t1 t2))
        in maybe cBase (\ t -> cBase :&: (t :=: t1)) mt))
generator (Let n mAnn e body) ty
  = CExists (\ t1 ->
      let
        c1 = generator e t1
        c1Ann = maybe c1 (\ sch -> c1 :&: CInstScheme sch t1) mAnn
        cBody = generator body ty
      in CLet n t1 c1Ann cBody)

typeOfLit :: Lit -> Type
typeOfLit (LInt _) = TInt
typeOfLit (LBool _) = TBool
