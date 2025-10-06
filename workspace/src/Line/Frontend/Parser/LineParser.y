{
module Line.Frontend.Parser.LineParser (lineParser, parserTest) where

import Line.Frontend.Lexer.Token
import Line.Frontend.Lexer.LineLexer hiding (lexer)
import Line.Frontend.Syntax.LineSyntax
}


%name parser Program
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}


%token
      var       {Token _ (TIdent $$)}
      num       {Token _ (TNumber $$)}
      ':='      {Token _ TAssign}
      ';'       {Token _ TSemi}
      'read'    {Token _ TRead}
      'print'   {Token _ TPrint}
      '('       {Token _ TLParen}
      ')'       {Token _ TRParen}
      '+'       {Token _ TPlus}
      '*'       {Token _ TTimes}

%left '+'
%left '*'

%%

Program : Line      {Line $1}

Line : Stmt Line    {$1 : $2}
     | {- empty -}  {[]}

Stmt : var ':=' Exp ';'        {SAssign $1 $3}
     | 'read' '(' Var ')' ';'  {SRead $3}
     | 'print' '(' Exp ')' ';' {SPrint $3}

Exp : num         { EInt $1 }
    | Var         { EVar $1 }
    | Exp '+' Exp { $1 :+: $3 }
    | Exp '*' Exp { $1 :*: $3 }
    | '(' Exp ')' { $2 }

Var : var         {$1}

{
parserTest :: String -> IO ()
parserTest s = do
  r <- lineParser s
  print r

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

lineParser :: String -> IO (Either String Line)
lineParser content = do
  pure $ runAlex content parser
}
