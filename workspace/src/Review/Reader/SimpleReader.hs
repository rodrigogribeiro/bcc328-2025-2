module Review.Reader.SimpleReader where 

newtype Reader e a 
  = Reader (e -> a) 

runReader :: Reader e a -> e -> a 
runReader (Reader f) = f 

instance Functor (Reader a) where
  fmap f (Reader g) = Reader (\ e -> f (g e))
  
instance Applicative (Reader a) where
  pure x = Reader $ \ _ -> x
  (Reader m) <*> (Reader n) = Reader $ \ e -> m e (n e)
  
instance Monad (Reader a) where
  (Reader m) >>= f = Reader $ \ e -> (runReader (f (m e))) e 
  
ask :: Reader a a
ask = Reader id
