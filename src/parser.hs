module Main (main) where

import Lexer
import Tokens
import Values
import Memory
import Statement
import Interpreter as Interpreter
import Text.Parsec
import Control.Monad.State
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe


-- invocação do parser para o símbolo de partida

printTokens = do
                contents <- readFile "programa.bra"
                print (scanTokens contents)

printParsedFile :: String -> IO ()
printParsedFile filepath = case unsafePerformIO (parser (getTokens filepath)) of
            { Left err -> print err;
                Right ans -> print ans
            }

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                        s <- hGetContents fh;
                        return (alexScanTokens s)}

parser :: [Token] -> IO (Either ParseError [Statement])
parser tokens = runParserT program [] "Failed" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "programa.bra")) of
            { Left err -> print err;
              Right ans -> evalStateT (evaluate ans) []
            }

main' :: String -> IO ()
main' filepath = case unsafePerformIO (parser (getTokens ("./problems/" ++ filepath))) of
            { Left err -> print err;
              Right ans -> evalStateT (evaluate ans) []
            }