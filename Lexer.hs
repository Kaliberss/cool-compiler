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
          
          
