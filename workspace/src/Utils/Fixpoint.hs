module Utils.Fixpoint where

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x
  = let x' = f x
    in if x' == x then x
       else fixedPoint f x'

