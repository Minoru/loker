{-# LANGUAGE ViewPatterns #-}
module IR2C ( ir2c )
where
import C
import IR

ir2c :: Statement -> ((CVariable CInt, CStatement), [CDeclaration])
ir2c = runDeclM . translateStatement

translateStatement :: Statement -> DeclM (CVariable CInt, CStatement)
translateStatement (Command (isArrayConstant -> Just argv)) = do
    status <- newScalarVar "status"
    argv_var <- newConstStringArrayNT "argv" argv
    return $ (status, CallRoutine $ RunCommand status argv_var)
translateStatement _ = error "translateStatement: unimplemented"
