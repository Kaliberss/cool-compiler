module Gen.State where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as B
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Reader
import Semantic(ClassEnv(..), MethodEnv(..), AttrEnv(..))
data CodeGenEnv = CodeGenEnv
    { classEnv :: ClassEnv
    , methodEnv :: MethodEnv
    , attrEnv :: AttrEnv
    , classTags :: Map String Int
    , currentCls :: String
    }

data CodeGenState = CodeGenState
    { regCount :: Int
      ,labelCount :: Int
      ,irBuffer :: [B.Builder]
      , stringConsts :: Map String String
    }

type CodeGen a = ReaderT CodeGenEnv (State CodeGenState) a 

emit :: B.Builder -> CodeGen ()
emit line = modify' $ \st -> st { irBuffer = line: irBuffer st}

freshReg :: CodeGen String
freshReg = do
    st <- get
    let r = regCount st
    put $ st { regCount = r+1}
    return ("%" ++ show r)

freshLabel :: String -> CodeGen String
freshLabel prefix = do
    st <- get
    let l = labelCount st
    put $ st {labelCount = l+1}
    return (prefix ++ "_" ++ show l)
