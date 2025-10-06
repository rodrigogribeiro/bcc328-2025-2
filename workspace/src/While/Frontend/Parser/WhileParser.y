{
module While.Frontend.Parser.WhileParser (whileParser, parserTest) where


import Utils.Value

import While.Frontend.Lexer.Token
import While.Frontend.Lexer.WhileLexer hiding (lexer)
import While.Frontend.Syntax.WhileSyntax
}


%name parser While
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
      'if'      {Token _ TIf}
      'else'    {Token _ TElse}
      'while'   {Token _ TWhile}
      'true'    {Token _ TTrue}
      'false'   {Token _ TFalse}
      '{'       {Token _ TLBrace}
      '}'       {Token _ TRBrace}
      '!'       {Token _ TNot}
      '||'      {Token _ TOr}
      '<'       {Token _ TLt}
      '='       {Token _ TEq}


%left '||'
%nonassoc '='
%nonassoc '<'
%left '+'
%left '*'
%left '!'

%%

While : '{' Stmts '}'   {While $2}

Stmts : Stmt Stmts        {$1 : $2}
      | {- empty -}       {[]}

Stmt : var ':=' Exp ';'        {SAssign $1 $3}
     | 'read' '(' Var ')' ';'  {SRead $3}
     | 'print' '(' Exp ')' ';' {SPrint $3}
     | 'if' '(' Exp ')' '{' Stmts '}' 'else' '{' Stmts '}' {SIf $3 $6 $10}
     | 'if' '(' Exp ')' '{' Stmts '}'                      {SIf $3 $6 []}
     | 'while' '(' Exp ')' '{' Stmts '}'  {SWhile $3 $6}

Exp : num         { EValue (VInt $1) }
    | 'true'      { EValue (VBool True) }
    | 'false'     { EValue (VBool False) }
    | Var         { EVar $1 }
    | '!' Exp     { ENot $2 }  
    | Exp '<' Exp { $1 :<: $3 }
    | Exp '=' Exp { $1 :=: $3 }
    | Exp '||' Exp { $1 :|: $3 }
    | Exp '+' Exp { $1 :+: $3 }
    | Exp '*' Exp { $1 :*: $3 }
    | '(' Exp ')' { $2 }

Var : var         {$1}

{
parserTest :: String -> IO ()
parserTest s = do
  r <- whileParser s
  print r

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

whileParser :: String -> IO (Either String While)
whileParser content = do
  pure $ runAlex content parser
}
