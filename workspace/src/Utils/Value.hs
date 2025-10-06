module Utils.Value where 

import Control.Monad.Except
import Utils.Pretty 

-- definition of values 

data Value 
  = VInt Int 
  | VBool Bool 
  | VString String 
  deriving (Eq, Ord, Show)

(.+.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .+. (VInt n2) = pure (VInt (n1 + n2))
v1 .+. v2 = throwError $ unlines ["Cannot sum:", pretty v1, "with", pretty v2]

(.*.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .*. (VInt n2) = pure (VInt (n1 * n2))
v1 .*. v2 = throwError $ unlines ["Cannot multiply:", pretty v1, "with", pretty v2]

(.<.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .<. (VInt n2) = pure (VBool (n1 < n2))
v1 .<. v2 = throwError $ unlines ["Cannot compare:", pretty v1, "with", pretty v2]

(.=.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .=. (VInt n2) = pure (VBool (n1 == n2))
(VBool b1) .=. (VBool b2) = pure (VBool (b1 == b2))
(VString s1) .=. (VString s2) = pure (VBool (s1 == s2))
v1 .=. v2 = throwError $ unlines ["Cannot compare:", pretty v1, "with", pretty v2]

(.|.) :: MonadError String m => Value -> Value -> m Value
(VBool b1) .|. (VBool b2) = pure (VBool (b1 || b2))
v1 .|. v2 = throwError $ unlines ["Cannot disjunct:", pretty v1, "with", pretty v2]

vnot :: MonadError String m => Value -> m Value 
vnot (VBool b) = pure (VBool (not b))
vnot e = throwError $ unlines ["Cannot negate:", pretty e]

instance Pretty Value where 
  ppr (VInt n) = int n 
  ppr (VBool b) = if b then text "true" else text "false"
  ppr (VString s) = doubleQuotes (text s)
