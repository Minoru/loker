{-# LANGUAGE ScopedTypeVariables,GeneralizedNewtypeDeriving #-}
-- This is the monad stack used for C code generation
--
-- The purpose of this module is to make the type abstract and to expose
-- domain-specific accessors

module CodeGenMonad
    ( CGM
    , runCGM
    , newVar
    , shellVarToID
    ) where

import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Applicative
import Data.Maybe
import Data.Function
import C -- for CDeclaration

-- At the moment our stack is represented by RWS (Reader, Writer, State) monad

newtype CGM a = CGM { unCGM :: RWS R W S a }
    deriving (Functor,Monad)

-- The Reader monad is used to hold the information about the
-- properties of the script being translated which are computed before the
-- generation phase and therefore never change.
-- Also it will be used to hold user's settings when there are any.

data R = R
    { shellVarsToIDs :: Map.Map String Int }

shellVarToID :: String -> CGM Int
shellVarToID var = CGM $ fromMaybe er . Map.lookup var <$> asks shellVarsToIDs
    where
    er = error $ "Var not known: " ++ var

-- The Writer monad is used to accumulate the variable declarations

data W = W
    { declarations :: [CDeclaration] }

instance Monoid W where
    mempty = W { declarations = [] }
    x `mappend` y = W
        { declarations = (mappend `on` declarations) x y }

-- There's no direct accessor to Writer; it's affected by 'newVar', which
-- also modifies the State

-- The State monad is used to generate fresh IDs for C variables
data S = S
    { nextCVarId :: !Int }
initialS = S { nextCVarId = 0 }

newVarN :: CGM Int
newVarN = CGM $ do
    n <- gets nextCVarId
    modify $ \s -> s { nextCVarId = n + 1 }
    return n

addDecl :: CDeclaration -> CGM ()
addDecl d = CGM $ tell $ mempty { declarations = [d] }

newVar :: forall t . CType t => String -> CGM (CVariable t)
newVar desc = CGM $ do
    n <- unCGM newVarN
    let var = CVariable n desc :: CVariable t
    unCGM $ addDecl $ CDeclaration var
    return var

runCGM :: CGM a -> Map.Map String Int -> (a, [CDeclaration])
runCGM m varMap =
    let (a,s,w) = runRWS (unCGM m) r initialS
    in (a, declarations w)
    where
    r = R { shellVarsToIDs = varMap }
