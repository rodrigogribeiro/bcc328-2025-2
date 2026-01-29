module MiniML.Frontend.Syntax.Type where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- definition of types

type Name = String

data Type
  = TInt
  | TBool
  | TFun Type Type
  | TVar Name
  deriving (Show, Eq, Ord)

-- substitution

type Subst = Map Name Type

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = Map.map (apply s1) s2 `Map.union` s1


class Apply a where
  ftv :: a -> Set Name
  apply :: Subst -> a -> a

instance Apply Type where
  ftv (TVar n) = Set.singleton n
  ftv (TFun t1 t2) = Set.union (ftv t1) (ftv t2)
  ftv _ = Set.empty


  apply s v@(TVar n)
    = maybe v id (Map.lookup n s)
  apply s (TFun t1 t2)
    = TFun (apply s t1) (apply s t2)
  apply _ t = t

data Scheme
  = Forall [Name] Type deriving (Ord, Show, Eq)

instance Apply Scheme where
  ftv (Forall vs t)
    = Set.difference (ftv t) (Set.fromList vs)
  apply s (Forall vs t)
    = Forall vs (apply s' t)
      where
        s' = Map.filterWithKey (\ v _ -> notElem v vs) s

type Ctx = Map Name Scheme

instance Apply Ctx where
  ftv = Map.foldr (\ sch ac -> Set.union (ftv sch) ac) Set.empty
  apply s = Map.map (apply s)
