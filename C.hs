{-# LANGUAGE GADTs, EmptyDataDecls #-}
module C
  (newIntVar, newConstStringArrayNT, C, DeclM, CVariable, CInt, CStringArrayNT, CStatement(..))
where
import Control.Monad.State
type C = String
data CInt
data CStringArrayNT -- NT stands for null-terminated

class CType a where
    typeToC :: a -> C

data CVariable typ = CVariable { varId :: Int }
    deriving Show

data CDeclaration
    -- integer variable
    = CDeclInt (CVariable CInt)
    -- constant null-terminated array of strings
    | CDeclConstArrStringNT (CVariable CStringArrayNT) [String]
    deriving Show

data DeclState = DeclState
    -- list of accumulated declarations (reverse order)
    { declarations :: [CDeclaration]
    -- number used to form the next variable name
    , nextVarN     :: Int
    }
    deriving Show

initialDeclState :: DeclState
initialDeclState = DeclState [] 0

type DeclM a = State DeclState a

data CStatement
    = RunCommand
        -- variable to write return status to
        -- if Nothing, pass NULL to waitpid
        (Maybe (CVariable CInt))
        -- variable which holds argv
        (CVariable CStringArrayNT)
    | CSequence [CStatement]
    | NoOp
    deriving Show

newVarN :: DeclM Int
newVarN = do
    n <- gets nextVarN
    modify $ \s -> s { nextVarN = n + 1 }
    return n

addDecl :: CDeclaration -> DeclM ()
addDecl d = modify $ \s -> s { declarations = d : declarations s }

newIntVar :: DeclM (CVariable CInt)
newIntVar = do
    n <- newVarN
    let var = CVariable n
    addDecl (CDeclInt var)
    return var

newConstStringArrayNT :: [String] -> DeclM (CVariable CStringArrayNT)
newConstStringArrayNT strs = do
    n <- newVarN
    let var = CVariable n
    addDecl (CDeclConstArrStringNT var strs)
    return var

genC :: DeclM (CVariable CInt, CStatement) -> C
genC d =
    let ((status, commands), decls) = runState d initialDeclState
    in error "Code generation needs to be written"
