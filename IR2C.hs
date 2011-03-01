{-# LANGUAGE ViewPatterns #-}
module IR2C ( ir2c )
where
import qualified Data.Map as Map
import C
import CHelpers
import IR
import CodeGenMonad

ir2c :: Statement -> ((CVariable CInt, CStatement), [CDeclaration])
ir2c = flip runCGM Map.empty . translateStatement

translateStatement :: Statement -> CGM (CVariable CInt, CStatement)
translateStatement (Command (isArrayConstant -> Just argv)) = do
    status <- newVar "status"
    argv_var <- newVar "argv"
    -- initialize argv
    let program = CSequence $
            [ AllocArray argv_var (length argv + 1) ] ++
            ( let init i arg = (argv_var ! (i::Int)) .= Strdup arg
              in zipWith init [0..] argv ) ++
            [ (argv_var ! length argv) .= NULL
            , status .= RunCommand argv_var
            ]
    return $ (status, program)
translateStatement _ = error "translateStatement: unimplemented"
