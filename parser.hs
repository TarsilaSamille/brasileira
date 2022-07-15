module Main (main) where

import Lexer
import Token
-- import Memory
import Statement
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe


-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- programToken 
            b <- idToken 
            c <- blockBeginToken "{"
            d <- stmts
            e <- blockEndToken "}"
            eof
            return (a:b:[c] ++ d ++ [e])


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "programa.bra")) of
            { Left err -> print err; 
              Right ans -> print ans
            }