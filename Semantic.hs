module Semantic where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad(zipWithM_)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.List (nub, intercalate)
import AST (Class (..), Expr(..), Feature(..), Formal(..))
import Lexer (Posicao(..))
import Text.Megaparsec hiding (State)

type ClassName = String
type IDName = String
type TypeName = String

-- no ambiente de uma classe, mapeamos uma classe ao seu pai
type ClassEnv = Map.Map ClassName ClassName

data MethodSig = MethodSig
    { formals :: [TypeName]
    , returnType :: TypeName
    } deriving (Show,Eq)

-- métodos são associados a sua classe, e portanto seu ambiente também
type MethodEnv = Map.Map ClassName (Map.Map IDName MethodSig)

type ObjectEnv = Map.Map IDName TypeName

data Env = Env
    { classEnv :: ClassEnv
    , methodEnv :: MethodEnv
    , objectEnv :: ObjectEnv
    , currentClass :: ClassName
    }
type TypeCheck a = ReaderT Env (State [String]) a 

baseClassEnv :: ClassEnv
baseClassEnv = Map.fromList [
     ("Object", ""),
     ("IO", "Object"),
     ("Int", "Object"),
     ("String", "Object"),
     ("Bool", "Object")
     ]

baseMethodEnv :: MethodEnv
baseMethodEnv = Map.fromList [
    ("Object", Map.fromList
        [ ("abort", MethodSig [] "Object"), ("type_name", MethodSig[] "String"), ("copy", MethodSig [] "SELF_TYPE")]),
    ("IO", Map.fromList
        [ ("out_string", MethodSig ["String"] "SELF_TYPE"), ("out_int", MethodSig["Int"] "SELF_TYPE"), ("in_string", MethodSig [] "String"),("in_int", MethodSig [] "Int")]),
    ("String", Map.fromList
        [ ("length", MethodSig [] "Int"), ("concat", MethodSig["String"] "String"), ("substr", MethodSig ["Int", "Int"] "String")] ),
    ("Int", Map.empty),
    ("Bool", Map.empty)
    ]

buildEnvs :: [Class Posicao] -> (ClassEnv, MethodEnv)
buildEnvs classes = (finalClassEnv, finalMethodEnv)
    where
        finalClassEnv = foldl' addClass baseClassEnv classes
        
        addClass env (Class _ className parentName _ ) = 
            Map.insert className (fromMaybe "Object" parentName) env

        finalMethodEnv = foldl' addClassMethods baseMethodEnv classes
        
        addClassMethods env (Class _ className _ features) = 
            let classMethods = foldl' addMethod Map.empty features
            in Map.insert className classMethods env

        addMethod env (Method _  methodName formals returnType _ ) =
            let getFormalType (Formal _ _ typeName) = typeName
                argTypes = map getFormalType formals
                signature = MethodSig argTypes returnType
            in Map.insert methodName signature env

        addMethod env (Attribute _ _ _ _) = env
                
        
findMethod :: ClassEnv -> MethodEnv -> ClassName -> IDName -> Maybe MethodSig
findMethod cEnv mEnv className methodName = 
    (Map.lookup className mEnv >>= Map.lookup methodName)
    <|>
    (case Map.lookup className cEnv of
        Just "Object" -> Nothing
        Just parentName -> findMethod cEnv mEnv parentName methodName
        Nothing -> Nothing
        )

    
validateHierarchy :: ClassEnv -> [String]
validateHierarchy env =
   nub $ concatMap checkClass (Map.keys env)
   where
    checkClass :: ClassName -> [String]
    checkClass start = walk start []

    walk :: ClassName -> [ClassName] -> [String]
    walk current visited 
            | current `elem` visited = 
                let cyclePath = intercalate " -> " (reverse (current:visited))
                in ["Erro semântico: Herança cíclica detectada: " ++ cyclePath]

            | current == "Object" = []
            | current == ""       = []

            | otherwise = case Map.lookup current env of
                Nothing -> ["Erro semântico: A classe " ++ current ++ " não foi definida."]
                Just parent -> if parent `elem` ["Int", "String", "Bool"]
                then ["Erro semântico: A classe " ++ current ++ " não deve herdar da classe " ++ parent]
                else walk parent (current:visited)

wrapPass1 :: [Class Posicao] -> Either [String] (ClassEnv, MethodEnv)
wrapPass1 ast = 
    let (cEnv,mEnv) = buildEnvs ast
        hierarchyErrors = validateHierarchy cEnv

        missingMainError =
            if Map.member "Main" cEnv
                then []
                else ["Erro semântico: O programa precisa de uma classe Main"]

        allErrors = hierarchyErrors ++ missingMainError
    
    in if null allErrors
        then Right(cEnv,mEnv)
        else Left(allErrors)

reportErrors :: String -> TypeCheck()
reportErrors msg = modify (\error -> error ++ [msg])

tempLocalVar :: String -> String -> TypeCheck a -> TypeCheck a
tempLocalVar varName varType = 
    local(\env ->
         let newEnv = Map.insert varName varType (objectEnv env)
         in env { objectEnv = newEnv} )


isSubtype :: ClassEnv -> String -> String -> String -> Bool
isSubtype classEnv currentClass t1 t2 
    | t1 == t2 = True
    | t2 == "Object" = True
    | t1 == "SELF_TYPE" = isSubtype classEnv currentClass currentClass t2
    | t2 == "SELF_TYPE" = False
    
    | otherwise = 
        case Map.lookup t1 classEnv of
            Just parent ->
                if parent == ""
                    then False
                    else isSubtype classEnv currentClass parent t2
            Nothing -> False

assertSubtype :: String -> String -> TypeCheck Bool
assertSubtype t1 t2 = do
    env <- ask
    return $ isSubtype (classEnv env) (currentClass env) t1 t2


createInheritancePath :: ClassEnv -> String -> [String]
createInheritancePath classEnv className = 
    className : case Map.lookup className classEnv of
        Just "" -> ["Object"] --potencialmente redundante
        Just "Object" -> ["Object"]
        Just parent -> createInheritancePath classEnv parent
        Nothing -> ["Object"]


findLowestAncestor :: ClassEnv -> String -> String -> String -> String
findLowestAncestor classEnv currentClass t1 t2 
    | t1 == t2 = t1 
    | otherwise = 
        let actualT1 = if t1 == "SELF_TYPE" then currentClass else t1
            actualT2 = if t2 == "SELF_TYPE" then currentClass else t2

            t1Path = createInheritancePath classEnv actualT1
            t2Path = createInheritancePath classEnv actualT2

            commonAncestors = [a | a <- t1Path, a `elem` t2Path]
        in head commonAncestors

calculateLowestAncestor :: String -> String -> TypeCheck String
calculateLowestAncestor t1 t2 = do
    env <- ask
    return $ findLowestAncestor (classEnv env) (currentClass env) t1 t2


typecheckExpr :: Expr Posicao -> TypeCheck String
typecheckExpr (ConstInt _ _ ) = return "Int"
typecheckExpr (ConstStr _ _)  = return "String"
typecheckExpr (BoolConst _ _) = return "Bool"

typecheckExpr (ConstId pos varName) = do
    if varName == "self"
        then return "SELF_TYPE"
        else do
            env <- asks objectEnv
            case Map.lookup varName env of
                Just t -> return t
                Nothing -> do
                    reportErrors $ "Linha " ++ show pos ++ ": identificador desconhecido" ++ varName
                    return "Object"

typecheckExpr (Assign pos varName expr) = do
    env <- ask

    let declaredType = Map.findWithDefault "Object" varName (objectEnv env)
    if Map.notMember varName (objectEnv env)
        then reportErrors $ "Linha " ++ show pos ++ ": variável desconhecida" ++ varName
        else return ()
    exprType <- typecheckExpr expr
    isValid <- assertSubtype exprType declaredType

    if not isValid
        then reportErrors $ "Linha " ++ show pos ++ ": Erro de tipo - Não é possível atribuir " ++ exprType ++ " para uma variável do tipo " ++ declaredType
        else return()
    return exprType

typecheckExpr (MethodCall pos caller methodName args) = do
    callerType <- typecheckExpr caller
    env <- ask

    let lookupClass = if callerType == "SELF_TYPE" then currentClass env else callerType
    
    case findMethod (classEnv env) (methodEnv env) lookupClass methodName of
        Nothing -> do
            reportErrors $ "Linha " ++ ": chamada a um método indefinido " ++ methodName ++ " no tipo " ++ lookupClass
            return "Object"

        Just (MethodSig formalTypes declaredReturnType) -> do
            if length args /= length formalTypes
                then do
                    reportErrors $ "Linha " ++ show pos ++ ": O método " ++ methodName ++ " esperava " ++ show (length formalTypes) ++ " argumentos, mas recebeu " ++ show (length args) 
                    return "Object"
                else do
                    argTypes <- mapM typecheckExpr args

                    zipWithM_(\actual formal -> do
                        isValid <- assertSubtype actual formal
                        if not isValid
                            then reportErrors $ "Linha " ++ show pos ++ ": o tipo do argumento é " ++ show actual ++ " mas o tipo esperado era " ++ formal
                            else return()
                            ) argTypes formalTypes

                    if declaredReturnType == "SELF_TYPE"
                    then return callerType
                    else return declaredReturnType 

                
                
typecheckExpr (If pos cond branch_then branch_else) = do
    condType <- typecheckExpr cond
    if condType /= "Bool"
        then reportErrors $ "Linha " ++ show pos ++ ": A condição do 'if' deve ser booleana, mas recebeu " ++ condType
        else return ()

    thenType <- typecheckExpr branch_then
    elseType <- typecheckExpr branch_else

    calculateLowestAncestor thenType elseType
                
                
                
                
                
                
                
                
                
                 
                   
                   
                        
                        
                        

    
