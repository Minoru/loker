{-# LANGUAGE GADTs, EmptyDataDecls, ExistentialQuantification,
    ScopedTypeVariables #-}
module C
    ( newScalarVar
    , newConstStringArrayNT
    , DeclM
    , runDeclM
    , CVariable
    , varId
    , varDesc
    , CInt
    , CType(..)
    , CScalarType
    , Routine(..)
    , CDeclaration(..)
    , CStringArrayNT
    , CStatement(..)
    )
where
import Control.Monad.State
import Control.Arrow
data CInt
data CStringArrayNT -- NT stands for null-terminated

class CType a where
    typeToString :: a -> String

class CType a => CScalarType a

instance CType CInt where typeToString _ = "int"

instance CScalarType CInt

data CVariable typ = CVariable
    { varId :: Int
    , varDesc :: String
    }
    deriving Show

data CDeclaration
    -- integer variable
    = forall t . CScalarType t => CDeclScalar (CVariable t)
    -- constant null-terminated array of strings
    | CDeclConstArrStringNT (CVariable CStringArrayNT) [String]

data DeclState = DeclState
    -- list of accumulated declarations (reverse order)
    { declarations :: [CDeclaration]
    -- number used to form the next variable name
    , nextVarN     :: !Int
    }

initialDeclState :: DeclState
initialDeclState = DeclState [] 0

type DeclM a = State DeclState a

data CStatement
    = CallRoutine Routine
    | CSequence [CStatement]
    | NoOp
    deriving Show

data Routine
    = RunCommand
        -- variable to write return status to
        (CVariable CInt)
        -- variable which holds argv
        (CVariable CStringArrayNT)
    deriving Show

newVarN :: DeclM Int
newVarN = do
    n <- gets nextVarN
    modify $ \s -> s { nextVarN = n + 1 }
    return n

addDecl :: CDeclaration -> DeclM ()
addDecl d = modify $ \s -> s { declarations = d : declarations s }

newScalarVar :: forall t . CScalarType t =>
                String -> DeclM (CVariable t)
newScalarVar desc = do
    n <- newVarN
    let var = CVariable n desc :: CVariable t
    addDecl $ CDeclScalar var
    return var

newConstStringArrayNT :: String -> [String] -> DeclM (CVariable CStringArrayNT)
newConstStringArrayNT desc strs = do
    n <- newVarN
    let var = CVariable n desc
    addDecl (CDeclConstArrStringNT var strs)
    return var

runDeclM :: DeclM a -> (a, [CDeclaration])
runDeclM m =
    second (reverse . declarations) $
        runState m initialDeclState
