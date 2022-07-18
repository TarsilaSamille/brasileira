{
  module Lexer where
}

%wrapper "posn"

$digit    = [0-9]
$graphic  = $printable # $white
$alpha    = [a-zA-Z]

tokens :-
  $white+                         ;
  "--".*                          ;
  
  principal { \p s -> Program (getLineColumn p)}

  (\( | \[ | \{) { \p s -> BlockBegin s (getLineColumn p)} 
  (\) | \] | \}) { \p s -> BlockEnd s (getLineColumn p)} 

  ":" { \p s -> Colon (getLineColumn p) }
  ";" { \p s -> SemiColon (getLineColumn p) }
  "," { \p s -> Comma (getLineColumn p) }
  
  "+=" { \p s -> PlusAssignment (getLineColumn p) }
  "-=" { \p s -> MinusAssignment (getLineColumn p) }
  "/=" { \p s ->  DivideAssignment (getLineColumn p) }
  "*=" { \p s -> MultiplyAssignment (getLineColumn p) }
  "=" { \p s -> Assignment (getLineColumn p) }
  "+" { \p s -> Plus (getLineColumn p) }
  "-" { \p s -> Minus (getLineColumn p) }
  "*" { \p s -> Multiply (getLineColumn p) }
  "**" { \p s -> Power (getLineColumn p) }
  "/" { \p s -> Divide (getLineColumn p) }
  "mod" { \p s -> Mod (getLineColumn p) }
  
  enquanto { \p s -> While (getLineColumn p) }
  para { \p s -> For (getLineColumn p) }
  retorne { \p s -> Return (getLineColumn p) }
  pare { \p s -> Break (getLineColumn p) }
  pass { \p s -> Pass (getLineColumn p) }
  switch { \p s -> Switch (getLineColumn p) }
  case { \p s -> Case (getLineColumn p) }
  funcao { \p s -> Function (getLineColumn p) }
  imprime { \p s -> Print (getLineColumn p) }
  imprimenl { \p s -> Println (getLineColumn p) }
  leia { \p s -> Read (getLineColumn p) }

  se { \p s -> If (getLineColumn p) }
  entao { \p s -> Else (getLineColumn p) }
  senao { \p s -> ElseIf (getLineColumn p) }
  e { \p s -> And (getLineColumn p) }
  ou { \p s -> Or (getLineColumn p) }
  nao { \p s -> Not (getLineColumn p) }
  "!=" { \p s -> NotEqual (getLineColumn p) }
  "==" { \p s -> Equal (getLineColumn p) }
  "<=" { \p s -> LessEqual (getLineColumn p) }
  "<" { \p s -> Less (getLineColumn p) }
  ">=" { \p s -> MoreEqual (getLineColumn p) }
  ">" { \p s -> More (getLineColumn p) }

  (inteiro|decimal|texto|caracter|logico|matrix) { \p s -> PrimitiveType s (getLineColumn p)}
  True {\p s -> ValueBool True (getLineColumn p) }
  False {\p s -> ValueBool False (getLineColumn p) }
  $digit+	{ \p s -> ValueInt (read s) (getLineColumn p) }
  $digit+\.$digit+ { \p s -> ValueFloat (read s) (getLineColumn p) }
  $alpha[$alpha $digit \_]*	  { \p s -> ID s (getLineColumn p) }
  \" [^\"]* \"  { \p s -> ValueString (read s) (getLineColumn p) }

{

-- The token type:
data Token =
             Program (Int, Int)    
           | BlockBegin String (Int, Int) 
           | BlockEnd String (Int, Int) 
           | PrimitiveType String (Int, Int) 
           | Colon (Int, Int)
           | SemiColon (Int, Int)
           | Comma (Int, Int)
           | PlusAssignment (Int, Int)
           | MinusAssignment (Int, Int)
           | DivideAssignment (Int, Int)
           | MultiplyAssignment (Int, Int)
           | Assignment (Int, Int)
           | Plus (Int, Int)
           | Minus (Int, Int)
           | Multiply (Int, Int)
           | Power (Int, Int)
           | Divide (Int, Int)
           | Mod (Int, Int)
           | While (Int, Int)
           | For (Int, Int)
           | Return (Int, Int)
           | Break (Int, Int)
           | Pass (Int, Int)
           | Switch (Int, Int)
           | Case (Int, Int)
           | Function (Int,Int)
           | Print (Int, Int)
           | Println (Int, Int)
           | Read (Int, Int)
           | If (Int, Int)
           | Else (Int, Int)
           | ElseIf (Int, Int)
           | And (Int, Int)
           | Or (Int, Int)
           | Not (Int, Int)
           | NotEqual (Int, Int)
           | Equal (Int, Int)
           | LessEqual (Int, Int)
           | Less (Int, Int)
           | MoreEqual (Int, Int)
           | More (Int, Int)
           | ID String (Int, Int)
           | ValueBool Bool (Int, Int)
           | ValueInt Int (Int, Int)
           | ValueFloat Double (Int, Int)
           | ValueString String (Int, Int)
             deriving (Eq,Show)

getLineColumn (AlexPn _ l c) = (l, c)

scanTokens s = alexScanTokens s

}