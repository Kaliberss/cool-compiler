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

parseInt :: Parser (Expr Posicao)
parseInt = do
    (pos, val) <- pIntToken
    return (ConstInt pos val)

parseString :: Parser (Expr Posicao)
parseString = do
    (pos,str) <- pStringToken 
    return (ConstStr pos str)

parseIf :: Parser (Expr Posicao)
parseIf = do
    pos <- parseHelper TokenIf "keyword if"
    cond <- parseExpr 
    _ <- parseHelper TokenThen "keyword then"
    branch_t <- parseExpr 
    _ <- parseHelper TokenElse "keyword else"
    branch_f <- parseExpr
    _ <- parseHelper TokenFi "keyword fi"

    return (If pos cond branch_t branch_f)

parseID :: Parser (Expr Posicao)
parseID = do
    (pos, str) <- pIDToken
    return (ConstId pos str)

parseParens :: Parser (Expr Posicao)
parseParens = between
    (parseHelper TokenLParen "'('")
    (parseHelper TokenRParen "')'")
    parseExpr

parseBlock :: Parser (Expr Posicao)

parseBlock = do
    pos <- parseHelper TokenLBrace "left brace '{'"
    exprs <- parseExpr `endBy1` parseHelper TokenSemi "semicolon ';'"
    _ <- parseHelper TokenRBrace "right brace '}'"
    return (Block pos exprs)

parseFalse :: Parser (Expr Posicao)
parseFalse = do
    pos <- parseHelper TokenFalse "keyword false"
    return (BoolConst pos False)

parseTrue :: Parser (Expr Posicao)
parseTrue = do
    pos <- parseHelper TokenTrue "keyword true"
    return (BoolConst pos True)


parseArgs :: Parser [Expr Posicao]
parseArgs = between
    (parseHelper TokenLParen "'('")
    (parseHelper TokenRParen "')")
    (parseExpr `sepBy` parseHelper TokenComma "',' between arguments")

parseSelfMethod :: Parser (Expr Posicao)
parseSelfMethod = try $ do
    (pos,name) <- pIDToken 
    args <- parseArgs
    return (MethodCall pos (ConstId pos "self") name args)

parseWhile :: Parser (Expr Posicao)
parseWhile = do
    pos <- parseHelper TokenWhile "keyword while"
    cond <- parseExpr
    _ <- parseHelper TokenLoop "keyword loop"
    body <- parseExpr
    _ <- parseHelper TokenPool "keyword pool"
    return (While pos cond body)

parseCaseBranch :: Parser (CaseStructure Posicao)
parseCaseBranch = do
    (pos,name) <- pIDToken
    _ <- parseHelper TokenColon "':'"
    (_, typeName) <- pTypeIdToken
    _ <- parseHelper TokenArrow "arrow"
    expr <- parseExpr
    _ <- parseHelper TokenSemi "';' at end of branch"
    return (CaseStructure pos name typeName expr)
    
parseBinding :: Parser (LetBinding Posicao)
parseBinding = do
    (pos,name) <- pIDToken
    _ <- parseHelper TokenColon "':'"
    (_, typeName) <- pTypeIdToken
    
    expr <- optional (do
        _ <- parseHelper TokenAssign "assign '<-'"
        parseExpr)
    return (LetBinding pos name typeName expr)

parseLet :: Parser (Expr Posicao)
parseLet = do
    pos <- parseHelper TokenLet "keyword let"
    bindings <- parseBinding `sepBy1` parseHelper TokenComma "',' between bindings"
    _ <- parseHelper TokenIn "keyword in"
    body <- parseExpr
    return (Let pos bindings body)

parseNot :: Parser (Expr Posicao)
parseNot = do
    pos <- parseHelper TokenNot "keyword not"
    expr <- parseExpr
    return (Neg pos expr)

parseCase :: Parser (Expr Posicao)
parseCase = do
    pos <- parseHelper TokenCase "keyword case"
    expr <- parseExpr
    _ <- parseHelper TokenOf "keyword of"
    branches <- some parseCaseBranch
    _ <- parseHelper TokenEsac "keyword esac"
    return (Case pos branches expr)

parseAssign :: Parser (Expr Posicao)
parseAssign =  do
    (pos,name) <- try $ do
        pID <- pIDToken
        _ <- parseHelper TokenAssign "assignment '<-'"
        return pID
    expr <- parseExpr
    return (Assign pos name expr)
   
   
   

parseFormal :: Parser (Formal Posicao)
parseFormal = do
    (pos,name) <- pIDToken 
    _ <- parseHelper TokenColon "':'"
    (_, typeName) <- pTypeIdToken 
    return (Formal pos name typeName)

parseMethod :: Parser (Feature Posicao)
parseMethod = do
    (pos,name) <- pIDToken 
    _ <- parseHelper TokenLParen "'('"
    formals <- parseFormal `sepBy` parseHelper TokenComma "',' between args"
    _ <- parseHelper TokenRParen "')'"
    _ <- parseHelper TokenColon "':'"
    (_, returnType) <- pTypeIdToken 

    _ <- parseHelper TokenLBrace "'{'"
    body <- parseExpr
    _ <- parseHelper TokenRBrace "'}'"
    _ <- parseHelper TokenSemi "';' at end of method"
    return (Method pos name formals returnType body)

parseDispatch :: Parser (Expr Posicao -> Expr Posicao)
parseDispatch = do
    _ <- parseHelper TokenDot "'.'"
    (pos,name) <- pIDToken
    _ <- parseHelper TokenLParen "'('"
    args <- parseExpr `sepBy` parseHelper TokenComma "','"
    _ <- parseHelper TokenRParen "')'"
    return (\obj -> MethodCall pos obj name args)

parseChainDispatch :: Parser (Expr Posicao -> Expr Posicao)
parseChainDispatch = do
    funcs <- some parseDispatch

    return (\obj -> foldl (\currentExpr func -> func currentExpr) obj funcs)

parseAttrib :: Parser (Feature Posicao)
parseAttrib = do
    (pos,name) <- pIDToken 
    _ <- parseHelper TokenColon "':'"
    (_, typeName) <- pTypeIdToken 

    expr <- optional (do
        _ <- parseHelper TokenAssign "'<-'"
        parseExpr)
    _ <- parseHelper TokenSemi "':' at end of attribute"

    return (Attribute pos name typeName expr)

parseFeature :: Parser (Feature Posicao)
parseFeature = try parseMethod <|> parseAttrib

parseNew :: Parser (Expr Posicao)
parseNew = do
    pos <- parseHelper TokenNew "keyword 'new'"
    (_, typeName) <- pTypeIdToken 
    return (New pos typeName)

operatorTable :: [[Operator Parser (Expr Posicao)]]
operatorTable = 
    [
        [ Postfix parseChainDispatch
          
        , Postfix (do
            _ <- parseHelper TokenAt "'@'"
            (_, name) <- pTypeIdToken 
            _ <- parseHelper TokenDot "'.'"
            (pos,methodName) <- pIDToken
            args <- parseArgs
            return (\expr -> MethodCallAt pos expr name methodName args))
        ]
    ,

        [ Prefix (do
            pos <- parseHelper TokenComplement "complement '~'"
            return (\expr -> Complement pos expr))
        ]
    ,   [ Prefix (do
            pos <- parseHelper TokenIsVoid "keyword 'isvoid'"
            return (\expr -> IsVoid pos expr))
        ]

    ,
        [ InfixL (do
            pos <- parseHelper TokenMult "operator '*'"
            return (\left right -> Mult pos left right))
        , InfixL (do
            pos <- parseHelper TokenDiv "operator '/'"
            return (\left right -> Div pos left right))
        ]
    ,
        [ InfixL (do
            pos <- parseHelper TokenPlus "operator '+'"
            return (\left right -> Add pos left right))
        , InfixL (do
            pos <- parseHelper TokenMinus "operator '-'"
            return (\left right -> Sub pos left right))
        ]
    ,
        [ InfixN (do
            pos <- parseHelper TokenLt "operator '<'"
            return (\left right -> Lt pos left right))
        , InfixN (do
            pos <- parseHelper TokenLe "operator '<='"
            return (\left right -> Le pos left right))
        , InfixN (do
            pos <- parseHelper TokenEq "operator '='"
            return (\left right -> Eq pos left right))
        ]
    ,   [ Prefix (do
            pos <- parseHelper TokenNot "keyword 'not'"
            return (\expr -> Neg pos expr))
        ]
    ]

parseExpr :: Parser (Expr Posicao)
parseExpr = try parseAssign <|> parseNonAssignExpr 
parseNonAssignExpr = makeExprParser parseTerm operatorTable

parseClass :: Parser (Class Posicao)
parseClass = do
    pos <- parseHelper TokenClass "keyword 'class'"
    (_, className) <- pTypeIdToken 
    parent <- optional (do
        _ <- parseHelper TokenInherits "keyword 'inherits'"
        (_, parentName) <- pTypeIdToken 
        return parentName)
    _ <- parseHelper TokenLBrace "'{'"

    features <- many parseFeature
    _ <- parseHelper TokenRBrace "'}'"
    _ <- parseHelper TokenSemi "';' at end of class"
    return (Class pos className parent features)

parseProgram :: Parser (Program Posicao)
parseProgram = do
    classes <- some parseClass

    return (Program classes)

parseTerm :: Parser(Expr Posicao)

parseTerm = choice
    [ parseIf
    , parseWhile
    , parseLet
    , parseCase
    , parseBlock
    , parseSelfMethod 
    , parseInt
    , parseString
    , parseTrue
    , parseFalse
    , parseParens
    , parseBlock
    , parseAssign
    , parseNew
    , parseID
    ]
    

