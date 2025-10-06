module While.Frontend.Lexer.Token where

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int
  | TLParen
  | TRParen
  | TPlus
  | TTimes
  | TIdent { out :: String }
  | TSemi
  | TRead
  | TPrint
  | TAssign
  | TIf 
  | TElse 
  | TWhile
  | TLt 
  | TEq
  | TNot 
  | TOr
  | TFalse
  | TTrue 
  | TLBrace 
  | TRBrace 
  | TEOF
  deriving (Eq, Ord, Show)


