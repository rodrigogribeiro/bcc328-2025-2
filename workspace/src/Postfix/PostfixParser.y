{
module Postfix.PostfixParser (expParser, parserTest) where

import Postfix.Token
import Postfix.PostfixLexer hiding (lexer)
import Postfix.PostfixSyntax
}

%name parser Exp
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}


%token
      num       {Token _ (TNumber $$)}
      '+'       {Token _ TPlus}
      '*'       {Token _ TTimes}

%left '+'
%left '*'

%expect 0
%%

Exp : num         { Const $1 }
    | Exp Exp '+' { Add $1 $2 }
    | Exp Exp '*' { Mul $1 $2 }

{
parserTest :: String -> IO ()
parserTest s = do
  r <- expParser s
  print r

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

expParser :: String -> IO (Either String Exp)
expParser content = do
  pure $ runAlex content parser
}
