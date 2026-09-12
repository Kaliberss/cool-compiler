module Lexer
( lexer
, Token(..)
, TipoToken(..)
, Posicao(..)
, LexErro(..)
)
where 

import Data.Char

data Posicao = Pos { line :: Int, col :: Int }
  deriving (Show,Eq,Ord)

data TipoToken
  = TokenClass | TokenTypeID String | TokenID String | TokenIf | TokenElse | TokenFi | TokenWhile | TokenLoop | TokenPool | TokenLet | TokenIn | TokenCase | TokenOf | TokenEsac | TokenNew | TokenIsVoid | TokenNot | TokenTrue | TokenFalse | TokenThen | TokenComplement | TokenPlus | TokenMinus | TokenDiv | TokenMult | TokenLt | TokenLe | TokenEq | TokenAssign | TokenInt Int| TokenString String| TokenInherits | TokenComma | TokenSemi | TokenColon | TokenLParen | TokenRParen | TokenLBrace | TokenRBrace | TokenDot | TokenArrow | TokenAt
  deriving (Show,Eq,Ord)

data Token = Token TipoToken Posicao
  deriving (Show,Eq,Ord)

data LexErro = LexErro String Posicao
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
          
          | sh == '(' && take 1 st == "*" = do
                (rest, newPos) <- consumirComentario (drop 1 st) (avancaPos pos '(') 1
                tokenizar rest newPos
          
          | sh == '"' = do
               (str, rest, newPos) <- consumirString st (avancaPos pos '"')[]
               (Token (TokenString str) pos :) <$> tokenizar rest newPos
          | isAlpha sh || sh == '_' =
              let (word, rest, newPos) = consumirID (sh:st) pos
              in (Token (classifyKeywordOrID word) pos:) <$> tokenizar rest newPos
          
          | isDigit sh =
              let (numStr, rest, newPos) = consumirWhile isDigit (sh:st) pos
              in (Token (TokenInt (read numStr)) pos :) <$> tokenizar rest newPos

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

classifyKeywordOrID :: String -> TipoToken
classifyKeywordOrID s = 
    let lowerStr = map toLower s 
    in case lowerStr of
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
        "isvoid"    -> TokenIsVoid
        "new"       -> TokenNew
        "loop"      -> TokenLoop
        "pool"      -> TokenPool
        "in"        -> TokenIn
        "while"     -> TokenWhile
        "then"      -> TokenThen
        "of"        -> TokenOf
        "not"       -> TokenNot
        _           -> if isUpper (head s) then TokenTypeID s else TokenID s

consumirString :: String -> Posicao -> String -> Either LexErro (String, String, Posicao)
consumirString [] pos _ = Left (LexErro "String contém EOF" pos)
consumirString ('"':st) pos acc = Right (reverse acc, st, avancaPos pos '"')
consumirString ('\0':st) pos _ = Left (LexErro "String contém caractér nulo" pos)
consumirString ('\n':_) pos _ = Left (LexErro "Uso inválido de newline" pos)
consumirString ('\\':sc:st) pos acc = 
    let slashChar = case sc of 
            'n'    -> '\n'
            't'    -> '\t'
            'b'    -> '\b'
            'f'    -> '\f'
            _      -> sc
        newPos = avancaPos (avancaPos pos '\\') sc 
    in consumirString st newPos (slashChar:acc)

consumirString (sc:st) pos acc = consumirString st (avancaPos pos sc) (sc:acc)



consumirID :: String -> Posicao -> (String, String, Posicao)

consumirID = consumirWhile(\ch -> isAlphaNum ch || ch == '_')

consumirWhile :: (Char -> Bool) -> String -> Posicao -> (String, String, Posicao)

consumirWhile p s pos = go s pos []
    where
      go [] pos_ acc = (reverse acc, [], pos_)
      go (sh:st) pos_ acc
        | p sh = go st (avancaPos pos_ sh) (sh:acc)
        | otherwise = (reverse acc, (sh:st), pos_)

matchSimbolo :: String -> (Maybe TipoToken, Int)

matchSimbolo ('(':st) = (Just TokenLParen, 1)
matchSimbolo (')':st) = (Just TokenRParen, 1)
matchSimbolo ('{':st) = (Just TokenLBrace, 1)
matchSimbolo ('}':st) = (Just TokenRBrace, 1)
matchSimbolo ('.':st) = (Just TokenDot, 1)
matchSimbolo ('-':st) = (Just TokenMinus, 1)
matchSimbolo ('+':st) = (Just TokenPlus,1)
matchSimbolo ('*':st) = (Just TokenMult,1)
matchSimbolo ('/':st) = (Just TokenDiv,1)
matchSimbolo ('@':st) = (Just TokenAt, 1)
matchSimbolo ('=': '>':st) = (Just TokenArrow, 2)
matchSimbolo ('=':st) = (Just TokenEq, 1)
matchSimbolo (',':st) = (Just TokenComma, 1)
matchSimbolo (';':st) = (Just TokenSemi,1)
matchSimbolo (':':st) = (Just TokenColon, 1)
matchSimbolo ('<':'-':st) = (Just TokenAssign, 2)
matchSimbolo ('<': '=':st) = (Just TokenLe, 2)
matchSimbolo ('<':st) = (Just TokenLt, 1)
matchSimbolo ('~':st) = (Just TokenComplement, 1)
matchSimbolo _ = (Nothing, 0)

consumirComentario :: String -> Posicao -> Int -> Either LexErro (String, Posicao)

consumirComentario [] pos _ = Left (LexErro "EOF no comentário" pos)
consumirComentario ('(':'*':st) pos depth = consumirComentario st (avancaPos (avancaPos pos '(') '*') (depth + 1)
consumirComentario ('*':')':st) pos 1 = Right (st, avancaPos (avancaPos pos '*') ')')
consumirComentario ('*':')':st) pos depth = consumirComentario st (avancaPos (avancaPos pos '*') ')') (depth -1)
consumirComentario (sh:st) pos depth = consumirComentario st (avancaPos pos sh) depth
