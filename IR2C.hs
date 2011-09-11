{-# LANGUAGE ViewPatterns #-}

-- Translation from Shell to C (or rather, between their intermediate
-- representations)
--
-- A lot of things here can be done more efficiently, but we are not concerned
-- with optimizations at the moment.

module IR2C ( ir2c )
where
import qualified Data.Map as Map
import Control.Monad
import C
import CHelpers
import IR
import qualified AST
import CodeGenMonad

ir2c :: Statement -> (CVariable CInt, [CDeclaration], CStatement)
ir2c = flip runCGM Map.empty . translateStatement

translateStatement :: Statement -> CGM (CVariable CInt)
translateStatement (Command argv) = do
    status <- newVar "status"
    (argv', _) <- translateArray argv
    status .= RunCommand argv'
    return status
translateStatement (Sequence cmds) = runCmds cmds
    where
      runCmds :: [Statement] -> CGM (CVariable CInt)
      runCmds (x:xs)
        | null xs = do
            status <- newVar "status"
            (status .=) =<< translateStatement x
            return status
        | otherwise = do
            translateStatement x
            runCmds xs
translateStatement (Pipeline commands) = do
    status <- newVar "status"
    cmds_var <- newVar "cmds"
    allocArray cmds_var (length commands + 1)
    let
        addCmd :: Int -> Statement -> CGM (CExpression CStringArrayNT)
        addCmd i (Command arg) = do
          (x, _) <- translateArray arg
          cmds_var ! i .= x
          return x
        addCmd _ x = error $ "addCmd in translateStatement for Pipeline: unimplemented for " ++ show x
    zipWithM addCmd [0..] commands
    cmds_var ! (length commands) .= NULL
    status .= PipelineCmds (length commands) cmds_var
    return status
translateStatement x = error $ "translateStatement: unimplemented for " ++ show x

-- The monadic result implicitly contains the statements to perform the
-- initialization if necessary
translateExpression :: Expression -> CGM (CExpression CString)
translateExpression (Const s) = return $ CExpression s -- string literal

-- This returns two C expressions: the array itself (NULL-terminated) and its
-- length.
--
-- Depending on the situation, we might not need the NULL terminator or the
-- length; so we may optimize generated code in the future. But now we don't
-- bother.
--
-- Also the monadic result implicitly contains the statements to perform the
-- initialization if necessary.
translateArray :: IR.Array -> CGM (CExpression CStringArrayNT, CExpression CInt)
translateArray (isArrayPlain -> Just exprs) = do
    let numExprs = length exprs
    arr <- newVar "array" :: CGM (CVariable CStringArrayNT)
    allocArray arr $ numExprs + 1
    let init i expr = arr ! (i::Int) .= Strdup expr
    translatedExprs <- mapM translateExpression exprs
    zipWithM init [0..] translatedExprs
    arr ! numExprs .= NULL
    return (CExpression arr, CExpression numExprs)
