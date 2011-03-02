{-# LANGUAGE ViewPatterns #-}

-- Translation from Shell to C (or rather, between their intermediate
-- representations)
--
-- A lot of things here can be done more efficiently, but we are not concerned
-- with optimizations at the moment.

module IR2C ( ir2c )
where
import qualified Data.Map as Map
import C
import CHelpers
import IR
import qualified AST
import CodeGenMonad

ir2c :: Statement -> (CVariable CInt, [CDeclaration], CStatement)
ir2c = flip runCGM Map.empty . translateStatement

translateStatement :: Statement -> CGM (CVariable CInt)
translateStatement (Command (isArrayConstant -> Just argv)) = do
    status <- newVar "status"
    argv_var <- newVar "argv"
    -- initialize argv
    allocArray argv_var (length argv + 1)
    let init i arg = (argv_var ! (i::Int)) .= Strdup arg
    sequence $ zipWith init [0..] argv
    (argv_var ! length argv) .= NULL
    status .= RunCommand argv_var
    return status
translateStatement _ = error "translateStatement: unimplemented"
