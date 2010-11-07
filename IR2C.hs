{-# LANGUAGE ViewPatterns #-}
module IR2C where
import C
import IR

translateStatement :: Statement -> DeclM (CVariable CInt, CStatement)
translateStatement (Command (isArrayConstant -> Just argv)) = do
    status <- newIntVar
    argv_var <- newConstStringArrayNT argv
    return $ (status, RunCommand (Just status) argv_var)
translateStatement _ = error "translateStatement: unimplemented"
