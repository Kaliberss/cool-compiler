module Parser where

import AST
import Lexer (Token(..), Posicao(..), TipoToken(..))
import Data.Void
import Text.Megaparsec hiding (Token)
import qualified Data.Set as Set
import Control.Monad.Combinators.Expr

type Parser = Parsec Void [Token]

matchToken :: (Token -> Maybe a) -> Parser a
matchToken test = token test Set.empty

parseHelper :: TipoToken -> String -> Parser Posicao

parseHelper expected label = matchToken test <?> label
    where
        test(Token t pos)
            | t == expected = Just pos
            | otherwise = Nothing

pIntToken :: Parser (Posicao, Int)
pIntToken = matchToken test <?> "int"
    where
        test (Token (TokenInt num) pos) = Just (pos,num)
        test _                         = Nothing

pIDToken :: Parser (Posicao, String)
pIDToken = matchToken test <?> "identifier"
    where
        test (Token (TokenID str) pos) = Just (pos, str)
        test _                         = Nothing

pTypeIdToken :: Parser (Posicao, String)
pTypeIdToken = matchToken test <?> "type identifier"
    where
        test (Token (TokenTypeID str) pos) = Just (pos, str)
        test _                             = Nothing

pStringToken :: Parser (Posicao, String)
pStringToken = matchToken test <?> "string literal"
    where
        test (Token (TokenString str) pos) = Just (pos, str)
        test _                             = Nothing 





















