module Semantic where

import qualified Data.Map as Map
import Control.Monad.Reader
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
