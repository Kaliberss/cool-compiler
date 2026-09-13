module AST where
import Data.Char
newtype Program a = Program [Class a]
    deriving (Show,Eq)

data Class a = Class a String (Maybe String) [Feature a]
    deriving (Show,Eq)

data Feature a 
    = Method String [(Formal a)] String (Expr a)
    | Attribute String String (Maybe (Expr a)) 
 deriving (Show,Eq)
data Formal a = Formal a String String
    deriving (Show,Eq)

data LetBinding a = LetBinding a String String (Maybe (Expr a))
    deriving(Show,Eq)

data CaseStructure a = CaseStructure a String String (Expr a)
    deriving (Show,Eq)
data Expr a
    = Assign a String (Expr a)
    | MethodCall a (Expr a) String [Expr a]
    | MethodCallAt a (Expr a) String String [Expr a]

    | If a (Expr a) (Expr a) (Expr a)
    | While a (Expr a) (Expr a)
    | Block a [Expr a]

    | Let a [LetBinding a] (Expr a)
    | Case a [CaseStructure a] (Expr a)

    | New a String
    | IsVoid a (Expr a)

    | Add a (Expr a) (Expr a)
    | Sub a (Expr a) (Expr a)
    | Mult a (Expr a) (Expr a)
    | Div a (Expr a) (Expr a)
    | Complement a (Expr a)

    | Lt a (Expr a) (Expr a)
    | Le a (Expr a) (Expr a)
    | Eq a (Expr a) (Expr a)
    | Neg a (Expr a) 

    | ConstId a String
    | ConstInt a Int
    | BoolConst a Bool
    | ConstStr a String

    deriving (Show,Eq)



    
            

