module Statement where

import Lexer
import Token
import Text.Parsec
-- import Memory
import Expression
import Data.Functor.Identity
import Control.Monad.IO.Class
import System.IO
import System.IO.Unsafe

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign <|> printlnStmt
          next <- remaining_stmts
          return (first ++ next) <|> (return [])

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO [Token]
remaining_stmts = (do a <- stmts
                      return a) <|> (return [])


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


-- print 

printlnStmt :: ParsecT [Token] [(Token,Token)] IO ([Token])
printlnStmt = do 
            a <- printToken
            b <- blockBeginToken "("
            c <- expression
            d <- blockEndToken ")"
            e <- semiColonToken
            s <- getState
            liftIO (print s)
            return (a:b:c:[d] ++ [e])

