module Semantic where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad(zipWithM_)
import Control.Monad(foldM)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.List (nub, intercalate)
import AST (Class (..), Expr(..), Feature(..), Formal(..),LetBinding(..),CaseStructure(..), Program(..))
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

type AttrEnv = Map.Map String (Map.Map String String)

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

buildAttrEnv :: ClassEnv -> [Class Posicao] -> Either [String] AttrEnv
buildAttrEnv cEnv classes = 
    let
        shallowEnv :: Map.Map String [(Posicao, String, String)]
        shallowEnv = Map.fromList [(name, getAttrs features) | Class _ name _ features <- classes ]

        getAttrs features = [ (pos,attrName, attrType) | Attribute pos attrName attrType _ <- features]

        processClass (accErrors, accEnv) (Class _ className _ _) = 

            let declared = Map.findWithDefault [] className shallowEnv

                ancestors = drop 1 (createInheritancePath cEnv className)

                inherited = concatMap (\anc -> Map.findWithDefault [] anc shallowEnv) ancestors
                
                inheritedNames = map (\(_, name, _) -> name) inherited

                shadowErrors = [ "Linha " ++ show (line pos) ++ ": Atributo '" ++ name ++ "' não pode redefinir um atributo herdado" | (pos,name, _) <- declared, name `elem` inheritedNames]

                inheritedMap = Map.fromList [ (name,t) | (_,name,t) <- inherited]
                declaredMap = Map.fromList [(name,t) | (_,name,t) <- declared]
                fullClassMap = Map.union declaredMap inheritedMap --left biased
            in (accErrors ++ shadowErrors, Map.insert className fullClassMap accEnv)
        (allErrors,finalAttrEnv) = foldl' processClass ([], Map.empty) classes
    in if null allErrors
        then Right finalAttrEnv
        else Left allErrors


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

wrapPass1 :: [Class Posicao] -> Either [String] (ClassEnv, MethodEnv, AttrEnv)
wrapPass1 ast = 
    let (cEnv,mEnv) = buildEnvs ast
        hierarchyErrors = validateHierarchy cEnv

        missingMainError =
            if Map.member "Main" cEnv
                then []
                else ["Erro semântico: O programa precisa de uma classe Main"]

        allErrors = hierarchyErrors ++ missingMainError
    
    in if not (null allErrors) 
        then Left allErrors
        else do
            aEnv <- buildAttrEnv cEnv ast
            Right (cEnv,mEnv,aEnv)

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

checkMath :: Posicao -> String -> Expr Posicao -> Expr Posicao -> TypeCheck String
checkMath pos operator e1 e2 = do
    t1 <- typecheckExpr e1
    t2 <- typecheckExpr e2
    if t1 /= "Int" || t2 /= "Int"
        then do 
            reportErrors $ "Linha " ++ show (line pos) ++ ": ambos operandos do " ++ operator ++ " devem ser do tipo Int, mas eram do tipo " ++ t1 ++ " e " ++ t2
            return "Int"

        else return "Int"

checkComparison :: Posicao -> String -> Expr Posicao -> Expr Posicao -> TypeCheck String
checkComparison pos operator e1 e2 = do
    t1 <- typecheckExpr e1
    t2 <- typecheckExpr e2
    if t1 /= "Int" || t2 /= "Int"
        then do 
            reportErrors $ "Linha " ++ show (line pos) ++ ": ambos operandos do " ++ operator ++ " devem ser do tipo Int, mas eram do tipo " ++ t1 ++ " e " ++ t2
            return "Bool"

        else return "Bool"

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
                    reportErrors $ "Linha " ++ show (line pos) ++ ": identificador desconhecido" ++ varName
                    return "Object"

typecheckExpr (Assign pos varName expr) = do
    env <- ask

    let declaredType = Map.findWithDefault "Object" varName (objectEnv env)
    if Map.notMember varName (objectEnv env)
        then reportErrors $ "Linha " ++ show (line pos) ++ ": variável desconhecida" ++ varName
        else return ()
    exprType <- typecheckExpr expr
    isValid <- assertSubtype exprType declaredType

    if not isValid
        then reportErrors $ "Linha " ++ show (line pos) ++ ": Erro de tipo - Não é possível atribuir " ++ exprType ++ " para uma variável do tipo " ++ declaredType
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
                    reportErrors $ "Linha " ++ show (line pos) ++ ": O método " ++ methodName ++ " esperava " ++ show (length formalTypes) ++ " argumentos, mas recebeu " ++ show (length args) 
                    return "Object"
                else do
                    argTypes <- mapM typecheckExpr args

                    zipWithM_(\actual formal -> do
                        isValid <- assertSubtype actual formal
                        if not isValid
                            then reportErrors $ "Linha " ++ show (line pos) ++ ": o tipo do argumento é " ++ show actual ++ " mas o tipo esperado era " ++ formal
                            else return()
                            ) argTypes formalTypes

                    if declaredReturnType == "SELF_TYPE"
                    then return callerType
                    else return declaredReturnType 

                
typecheckExpr (MethodCallAt pos caller staticType methodName args) = do
    callerType <- typecheckExpr caller

    isValidCast <- assertSubtype callerType staticType
    if not isValidCast
        then do
            reportErrors $ "Linha " ++ show (line pos) ++ ": Tipo à esquerda do @ (" ++ callerType ++ ") deve conformar ao tipo à direita do @ (" ++ staticType ++ ")"
            return "Object"
        else do
            env <- ask
            
            case findMethod (classEnv env) (methodEnv env) staticType methodName of
                Nothing -> do
                    reportErrors $ "Linha " ++ show (line pos) ++ ": O método " ++ methodName ++ " não foi definido na classe " ++ staticType
                    return "Object"

                Just (MethodSig formalTypes declaredReturnType) -> do
                    
                    if length formalTypes /= length args
                        then do
                            reportErrors $ "Linha " ++ show (line pos) ++ ": O método " ++ methodName ++ " esperava " ++ show (length formalTypes) ++ " mas recebeu " ++ show (length args)
                            return "Object"
                        else do
                            argTypes <- mapM typecheckExpr args
                            zipWithM_ (\actual formal -> do
                                isValidArg <- assertSubtype actual formal
                                if not isValidArg
                                    then reportErrors $ "Linha " ++ show (line pos) ++ ": O tipo do argumento (" ++ actual ++ ")" ++ " não conforma ao esperado (" ++ formal ++ ")"
                                    else return ()
                                        ) argTypes formalTypes

                            if declaredReturnType == "SELF_TYPE"
                                then return callerType
                                else return declaredReturnType


                    




    
typecheckExpr (If pos cond branch_then branch_else) = do
    condType <- typecheckExpr cond
    if condType /= "Bool"
        then reportErrors $ "Linha " ++ show (line pos) ++ ": A condição do 'if' deve ser booleana, mas recebeu " ++ condType
        else return ()

    thenType <- typecheckExpr branch_then
    elseType <- typecheckExpr branch_else

    calculateLowestAncestor thenType elseType
                

typecheckExpr (Let _ bindings body) = do
    let processBindings [] = typecheckExpr body
        processBindings ((LetBinding bindPos varName varType inExpr) : rest) = do
            case inExpr of
                Just expr -> do
                    exprType <- typecheckExpr expr
                    isValid <- assertSubtype exprType varType
                    if not isValid
                        then reportErrors $ "Linha " ++ show bindPos ++ ": o tipo da expressão " ++ exprType ++ "não é do tipo esperado, " ++ varType
                        else return ()

                Nothing -> return ()
                
            tempLocalVar varName varType (processBindings rest)

    processBindings bindings

typecheckExpr (Block pos exprs) = do
    types <- mapM typecheckExpr exprs
    return (last types)

typecheckExpr (Case pos testExpr branches) = do
    _ <- typecheckExpr testExpr
    let declaredTypes = map(\(CaseStructure _ _ t _) -> t) branches
    if length (nub declaredTypes) /= length declaredTypes
        then reportErrors $ "Linha " ++ show (line pos) ++ ": Tipos duplicados em branches do case"
        else return ()

    let typecheckBranch (CaseStructure _ varName varType body) = 
            tempLocalVar varName varType (typecheckExpr body)

    branchTypes <- mapM typecheckBranch branches

    let (firstType : restTypes) = branchTypes
    foldM calculateLowestAncestor firstType restTypes


typecheckExpr (While pos cond body) = do
    condType <- typecheckExpr cond
    if condType /= "Bool"
        then reportErrors $ "Linha " ++ show (line pos) ++ ": a condição do while deveria ser um Bool, mas foi " ++ condType
        else return ()

    _ <- typecheckExpr body

    return "Object"

  
typecheckExpr (Add pos e1 e2) = checkMath pos "+" e1 e2
typecheckExpr (Sub pos e1 e2) = checkMath pos "-" e1 e2
typecheckExpr (Mult pos e1 e2) = checkMath pos "*" e1 e2
typecheckExpr (Div pos e1 e2) = checkMath pos "/" e1 e2

typecheckExpr (Lt pos e1 e2) = checkComparison pos "<" e1 e2
typecheckExpr (Le pos e1 e2) = checkComparison pos "<=" e1 e2

typecheckExpr (Eq pos e1 e2) = do
    t1 <- typecheckExpr e1
    t2 <- typecheckExpr e2

    let isBasicType t = t `elem` ["Int", "String", "Bool"]

    if (isBasicType t1 || isBasicType t2) && (t1 /= t2)
        then do
            reportErrors $ "Linha " ++ show (line pos) ++ ": Não é possível comparar " ++ t1 ++ " com " ++ t2
            return "Bool"

        else return "Bool"

typecheckExpr (IsVoid pos expr) = do
    _ <- typecheckExpr expr
    return "Bool"

typecheckExpr (New pos typeName) = do
    return typeName

typecheckExpr (Complement pos expr) = do
    t <- typecheckExpr expr
    if t /= "Int"
        then do
            reportErrors $ "Linha " ++ show (line pos) ++ ": O operando de ~ deve ser um Int, mas foi um " ++ t
            return "Int"

        else return "Int"
    

typecheckFeature :: ClassEnv -> MethodEnv -> AttrEnv -> String -> Feature Posicao -> TypeCheck ()

typecheckFeature cEnv mEnv aEnv className (Method pos methodName formals returnType body) = do
    let formalsList = map (\(Formal _ name t) -> (name, t)) formals

    local (\env -> env {objectEnv = Map.union (Map.fromList formalsList) (objectEnv env) }) $ do
        bodyType <- typecheckExpr body

        let expectedType = if returnType == "SELF_TYPE" then className else returnType
        let actualType = if bodyType == "SELF_TYPE" then className else bodyType
        isValidReturn <- assertSubtype actualType expectedType
        if not isValidReturn
            then reportErrors $ "Linha " ++ show (line pos) ++ ": Tipo de retorno do método " ++ methodName ++ "não conforma ao declarado (" ++ returnType ++ ")"
            else return ()

typecheckFeature cEnv mEnv aEnv className (Attribute pos attrName attrType initExpr) = do
    case initExpr of
        Just expr -> do
            initType <- typecheckExpr expr
            isValid <- assertSubtype initType attrType
            if not isValid
                then do
                    reportErrors $ "Linha " ++ show (line pos) ++ ": Tipo da inicialização do atributo " ++ attrName ++ "(" ++ initType ++ ")" ++ "não conforma a " ++ attrType
                else return ()

        Nothing -> return ()
    


wrapPass2 :: [Class Posicao] -> ClassEnv -> MethodEnv -> AttrEnv -> Either [String] ()
wrapPass2 ast cEnv mEnv aEnv =
    let initialEnv = Env {
        classEnv = cEnv,
        methodEnv = mEnv,
        objectEnv = Map.singleton "self" "SELF_TYPE",
        currentClass = "Object"
    }
        typecheckClass (Class _ className _ features) = do
            local (\env -> env {currentClass = className}) $ do
      
                let classAttrs = Map.findWithDefault Map.empty className aEnv
                local (\env -> env {objectEnv = Map.union classAttrs (objectEnv env)}) $ do
                    mapM_ (typecheckFeature cEnv mEnv aEnv className) features
                   
        typecheckAllClasses = mapM_ typecheckClass ast

        (_, finalErrors) = runState (runReaderT typecheckAllClasses initialEnv) []

    in if null finalErrors 
        then Right ()
        else Left finalErrors
        
semanticAnalysis :: [Class Posicao] -> Either [String] ()

semanticAnalysis ast = do 
    (cEnv,mEnv,aEnv) <- wrapPass1 ast
    wrapPass2 ast cEnv mEnv aEnv


                

                
                
                
                
                
                
                
                 
                   
                   
                        
                        
                        

    
