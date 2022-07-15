module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  get_token (Program p) = Just (Program p)
  get_token _           = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _        = Nothing

beginToken = tokenPrim show update_pos get_token where
  get_token (Begin p) = Just (Begin p)
  get_token _         = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p) 
  get_token _       = Nothing

semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _             = Nothing
  
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _          = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int x p) = Just (Int x p)
  get_token _         = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token (Type x p) = Just (Type x p)
  get_token _          = Nothing 

addToken = tokenPrim show update_pos get_token where
  get_token (Add p) = Just (Add p)
  get_token _       = Nothing 

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- programToken 
            b <- idToken 
            c <- beginToken 
            d <- stmts
            e <- endToken
            eof
            return (a:b:[c] ++ d ++ [e])


stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_stmts = (do a <- assign
                      return (a)) <|> (return [])


assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          d <- typeToken
          a <- idToken
          b <- assignToken
          c <- expression
          e <- semiColonToken
          s <- getState
          updateState(symtable_insert (a, get_default_value d))
          updateState(symtable_update (a, c))
          liftIO (print s)
          return (d:a:b:[c])

-- funções para verificação de tipos

get_default_value :: Token -> Token
get_default_value (Type "int" (l, c)) = Int 0 (l, c)

get_type :: Token -> [(Token, Token)] -> Token
get_type _ [] = error "variable not found"
get_type (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                             else get_type (Id id1 p1) t

compatible :: Token -> Token -> Bool
compatible (Int _ _) (Int _ _) = True
compatible _ _ = False

-- funções para o avaliador de expressões

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
una_expression = do
                   op <- addToken
                   a <- intToken 
                   return (a)
   
--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   n1 <- intToken
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining n1 = do
                      op <- addToken
                      n2 <- intToken
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)                              

eval :: Token -> Token -> Token -> Token
eval (Int x p) (Add _ ) (Int y _) = Int (x + y) p

-- funções para a tabela de símbolos

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (Id id1 p1, v1) ((Id id2 p2, v2):t) = 
                               if id1 == id2 then (Id id1 p2, v1) : t
                               else (Id id2 p2, v2) : symtable_update (Id id1 p1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "programa.bra")) of
            { Left err -> print err; 
              Right ans -> print ans
            }