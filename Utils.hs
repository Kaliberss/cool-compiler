module Utils where

import Text.Megaparsec hiding (Token)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Data.List (intercalate)
import Data.Void (Void)
import Lexer(Token(..), Posicao(..))


formatAllErrors :: ParseErrorBundle [Token] Void -> String
formatAllErrors bundle = 
    let allErrors = NE.toList (bundleErrors bundle)
        formattedErrors = map formatError allErrors
    in "Foram encontrados " ++ show (length allErrors) ++ " erros:\n" ++ intercalate "-----------------------\n" formattedErrors

formatError :: ParseError [Token] Void -> String

formatError err = 
     case err of 
        TrivialError _ unexpected expected ->
            let
                unexpStr = case unexpected of
                    Just item -> formatErrorItem item
                    Nothing -> "fim da entrada"

                expStr = if Set.null expected
                         then "nada"
                         else intercalate ", " (map formatErrorItem (Set.toList expected))

                pos =    case unexpected of 
                    Just (Tokens t) -> extrairPos (NE.head t)
                    _              -> "posição desconhecida"
            
            in "Erro em " ++ pos ++ "\n" ++ " Valor recebido: " ++ unexpStr ++ "\n" ++ " Valor esperado: " ++ expStr

        FancyError _ _ -> "Erro não identificado"

formatErrorItem :: ErrorItem Token -> String
formatErrorItem (Tokens t) = show (NE.head t)
formatErrorItem (Label l) = NE.toList l
formatErrorItem EndOfInput = "EOF"

extrairPos :: Token -> String

extrairPos token = 
    case token of
        Token _ pos -> "Linha " ++ show (line pos) ++ ", coluna " ++ show (col pos)
        _ -> show token

