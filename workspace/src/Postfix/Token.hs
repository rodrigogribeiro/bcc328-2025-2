module Postfix.Token where

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int
  | TPlus
  | TTimes
  | TEOF
  deriving (Eq, Ord, Show)


