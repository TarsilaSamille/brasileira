module Token where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Data.Functor.Identity
import System.IO.Unsafe

-- parsers para os tokens

programToken :: ParsecT [Token] st IO Token
programToken = tokenPrim show update_pos get_token where
  get_token (Program p) = Just (Program p)
  get_token _           = Nothing

idToken :: ParsecT [Token] st IO Token
idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _        = Nothing

semiColonToken :: ParsecT [Token] st IO Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _             = Nothing

assignToken :: ParsecT [Token] st IO Token  
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _          = Nothing

intToken :: ParsecT [Token] st IO Token  
intToken = tokenPrim show update_pos get_token where
  get_token (Int x p) = Just (Int x p)
  get_token _         = Nothing

typeToken :: ParsecT [Token] st IO Token  
typeToken = tokenPrim show update_pos get_token where
  get_token (Type x p) = Just (Type x p)
  get_token _          = Nothing 

addToken :: ParsecT [Token] st IO Token  
addToken = tokenPrim show update_pos get_token where
  get_token (Add p) = Just (Add p)
  get_token _       = Nothing 

printToken :: ParsecT [Token] st IO Token  
printToken = tokenPrim show update_pos get_token where
  get_token (Print p) = Just (Print p)
  get_token _          = Nothing

--keywordToken :: String -> ParsecT [Token] st Data.Functor.Identity.Identity Token  
keywordToken stmt = tokenPrim show update_pos get_token where
    get_token (Keyword x p) = if x == stmt then Just (Keyword x p) else Nothing
    get_token _         = Nothing

-- blockBeginToken :: String -> ParsecT [Token] st Data.Functor.Identity.Identity Token
blockBeginToken stmt = tokenPrim show update_pos get_token where
    get_token (BlockBegin x p) = if x == stmt then Just (BlockBegin x p) else Nothing
    get_token _         = Nothing    

-- blockEndToken :: String -> ParsecT [Token] st Data.Functor.Identity.Identity Token
blockEndToken stmt = tokenPrim show update_pos get_token where
    get_token (BlockEnd x p) = if x == stmt then Just (BlockEnd x p) else Nothing
    get_token _         = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  
