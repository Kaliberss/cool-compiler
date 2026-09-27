module Semantic where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State

type ClassName = String
type IDName = String
type TypeName = String

-- no ambiente de uma classe, mapeamos uma classe ao seu pai
type ClassEnv = Map.Map ClassName ClassName

data Method = Method 
    { args :: [TypeName]
    , returnType :: TypeName
    } deriving (Show,Eq)

-- métodos são associados a sua classe, e portanto seu ambiente também
type MethodEnv = Map.Map ClassName (Map.Map IDName Method)

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
        [ ("abort", Method [] "Object"), ("type_name", Method [] "String"), ("copy", Method [] "SELF_TYPE")]),
    ("IO", Map.fromList
        [ ("out_string", Method ["String"] "SELF_TYPE"), ("out_int", Method ["Int"] "SELF_TYPE"), ("in_string", Method [] "String"),("in_int", Method [] "Int")]),
    ("String", Map.fromList
        [ ("length", Method [] "Int"), ("concat", Method ["String"] "String"), ("substr", Method ["Int", "Int"] "String")] ),
    ("Int", Map.empty),
    ("Bool", Map.empty)
    ]

 
    

