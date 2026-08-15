module Lexer where 

import Data.Char

data TipoToken
  = TokenClass | TokenTypeID | TokenID | TokenIf | TokenElse | TokenFi | TokenWhile | TokenLoop | TokenPool | TokenLet | TokenIn | TokenCase | TokenOf | TokenEsac | TokenNew | TokenIsVoid | TokenNot | TokenTrue | TokenFalse | TokenThen | TokenComplement | TokenPlus | TokenMinus | TokenDiv | TokenMult | TokenLt | TokenLe | TokenEq | TokenAssign | TokenInt | TokenString | TokenInherits | TokenComma | TokenSemi | TokenColon | TokenLParen | TokenRParen | TokenLBrace | TokenRBrace | TokenDot | TokenArrow
  deriving (Show,Eq)
