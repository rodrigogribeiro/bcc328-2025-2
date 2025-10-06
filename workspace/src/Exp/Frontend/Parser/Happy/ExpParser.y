{
module Exp.Frontend.Parser.Happy.ExpParser (expParser, parserTest) where

import Exp.Frontend.Lexer.Token
import Exp.Frontend.Lexer.Alex.ExpLexer hiding (lexer)
import Exp.Frontend.Syntax.ExpSyntax
}


%name parser Exp
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}


%token
      num       {Token _ (TNumber $$)}
      '('       {Token _ TLParen}
      ')'       {Token _ TRParen}
      '+'       {Token _ TPlus}
      '*'       {Token _ TTimes}

%left '+'
%left '*'

%%

Exp : num         { EInt $1 }
    | Exp '+' Exp { $1 :+: $3 }
    | Exp '*' Exp { $1 :*: $3 }
    | '(' Exp ')' { $2 }

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
