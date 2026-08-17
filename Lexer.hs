module Lexer where 

import Data.Char

data Posicao = Pos { line :: Int, col :: Int }
  deriving (Show,Eq)

data TipoToken
  = TokenClass | TokenTypeID | TokenID | TokenIf | TokenElse | TokenFi | TokenWhile | TokenLoop | TokenPool | TokenLet | TokenIn | TokenCase | TokenOf | TokenEsac | TokenNew | TokenIsVoid | TokenNot | TokenTrue | TokenFalse | TokenThen | TokenComplement | TokenPlus | TokenMinus | TokenDiv | TokenMult | TokenLt | TokenLe | TokenEq | TokenAssign | TokenInt | TokenString | TokenInherits | TokenComma | TokenSemi | TokenColon | TokenLParen | TokenRParen | TokenLBrace | TokenRBrace | TokenDot | TokenArrow
  deriving (Show,Eq)

data Token = Token TipoToken 
  deriving (Show,Eq)

data LexErro = LexErro String 
  deriving (Show, Eq)

lexer :: String -> Either LexErro [Token]

lexer src = tokenizar src (Pos 1 1)
    where
        tokenizar :: String -> Posicao -> Either LexErro [Token]
        tokenizar [] _ = Right []
        
        --whitespace
        tokenizar ('\n':st) (Pos l _) = tokenizar st (Pos(l+1) 1)
        tokenizar (sh:st) pos@(Pos l c)
          | sh == ' ' || sh == '\t' = tokenizar st (Pos l (c+1))
          
          --comentarios de uma linha
          | sh == '-' && take 1 st == "-" = 
            let droppedcomm = dropWhile (/= '\n') st
            in tokenizar droppedcomm pos
          
          
          | sh == '"' = do
               (str, rest, newPos) <- consumirString st (avancaPos pos '"')[]
               (Token (TokenString str) pos :) <$> tokenizar rest newPos
          | isAlpha sh || sh == '_' =
              let (word, rest, newPos) = consumeIdent (sh:st) pos
              in (Token (classifyKeywordOrID word) pos:) <$> tokenizar rest
          
          | isDigit sh =
              let (numStr, rest, newPos) = consumeWhile isDigit (sh:st) pos
              in (Token (TokenInt (read numStr)) pos :) <$> tokenize rest newPos

          | otherwise = 
             let (tokenTipo, qtAvanco) = matchSimbolo (sh:st)
             in case tokenTipo of
                  Just tt -> 
                    let newPos = foldl avancaPos pos (take qtAvanco (sh:st))
                    in (Token tt pos :) <$> tokenizar (drop qtAvanco (sh:st)) newPos
                  Nothing -> Left (LexErro ("Caractér inválido: " ++ [sh]) pos)

avancaPos :: Posicao -> Char -> Posicao
avancaPos (Pos l c) '\n' = Pos (l+1) 1
avancaPos (Pos l c) _ = Pos l (c+1)

classifyKeywordOrID :: String -> TokenTipo
classifyKeywordOrID s = 
    let lowerStr = map toLower s 
    in case lowerS of
        "class"     -> TokenClass
        "else"      -> TokenElse
        "let"       -> TokenLet
        "false"     -> TokenFalse
        "if"        -> TokenIf
        "fi"        -> TokenFi
        "true"      -> TokenTrue
        "case"      -> TokenCase
        "esac"      -> TokenEsac
        "inherits"  -> TokenInherits

