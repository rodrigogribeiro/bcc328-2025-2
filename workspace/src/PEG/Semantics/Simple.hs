{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
module PEG.Semantics.Simple where 

import Control.Applicative
import Control.Monad (MonadPlus(..), guard)
import Data.Char (isDigit, ord, isSpace)

newtype PExp d a 
  = PExp { 
      runPExp :: d -> Result d a 
    } deriving Functor
 
data Result d a
  = Pure a             -- didn't consume anything, can backtrack
  | Commit d a         -- consumed input
  | Fail String Bool   -- failed, flagged if consumed
  deriving Functor

instance Applicative (PExp d) where
  pure a = PExp $ \ _ -> Pure a
  PExp mf <*> PExp ma 
    = PExp $ \ d -> 
      case mf d of
        Pure f      -> fmap f (ma d)
        Fail s c    -> Fail s c
        Commit d' f -> 
          case ma d' of
            Pure a       -> Commit d' (f a)
            Fail s _     -> Fail s True
            Commit d'' a -> Commit d'' (f a)

instance Alternative (PExp d) where
  PExp ma <|> PExp mb 
    = PExp $ \ d -> 
      case ma d of
        Fail _ False -> mb d
        x            -> x
  empty = PExp $ \ _ -> Fail "empty" False


instance Monad (PExp d) where
  PExp m >>= k = PExp $ \d -> 
    case m d of
      Pure a -> runPExp (k a) d
      Commit d' a -> 
        case runPExp (k a) d' of
          Pure b -> Commit d' b
          Fail s _ -> Fail s True
          commit -> commit
      Fail s c -> Fail s c
 
instance MonadPlus (PExp d) where
  mplus = (<|>)
  mzero = empty

try :: PExp d a -> PExp d a
try (PExp m) 
  = PExp $ \d -> 
      case m d of
        Fail s _ -> Fail s False
        x        -> x

infixl 3 </>

(</>) :: PExp d a -> PExp d a -> PExp d a
p </> q = try p <|> q


class Stream d where
  anyChar :: PExp d Char

instance Stream [Char] where
  anyChar = PExp $ \s -> case s of
    (x:xs) -> Commit xs x
    [] -> Fail "EOF" False

satisfy :: Stream d => (Char -> Bool) -> PExp d Char
satisfy p = try $ do
  x <- anyChar
  x <$ guard (p x)

whiteSpace :: Stream d => PExp d ()
whiteSpace = () <$ many (satisfy isSpace)

phrase :: Stream d => PExp d a -> PExp d a
phrase m = whiteSpace *> m <* eof
 
notFollowedBy :: PExp d a -> PExp d ()
notFollowedBy (PExp m) 
  = PExp $ \d -> 
      case m d of
        Fail{} -> Pure ()
        _      -> Fail "unexpected" False
 
eof :: Stream d => PExp d ()
eof = notFollowedBy anyChar


char :: Stream d => Char -> PExp d Char
char c = satisfy (c ==)
 
lexeme :: Stream d => PExp d a -> PExp d a
lexeme m = m <* whiteSpace
 
symbol :: Stream d => Char -> PExp d Char
symbol c = lexeme (char c)
 
digit :: Stream d => PExp d Int
digit 
  = f <$> satisfy isDigit
    where 
      f c = ord c - ord '0'
