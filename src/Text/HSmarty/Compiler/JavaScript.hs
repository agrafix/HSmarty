{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Text.HSmarty.Compiler.JavaScript
    ( compile )
where

import Text.HSmarty.Types

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import System.Random

type CompM a = ErrorT T.Text IO a

instance Error T.Text where
    strMsg = T.pack

compile :: Smarty -> CompM T.Text
compile (Smarty name (x:tpl)) =
    case x of
      (SmartyComment val) ->
          do let funName = T.filter isAlphaNum val
                 funHead = T.concat [ "function ", funName, "(tplArgs) { var s = ''; "]
                 funTail = T.concat [ "return s; }" ]
             body <- mapM compileStmt tpl
             return $ T.concat [funHead, T.concat body, funTail]
      _ ->
          throwError "The first line must be a comment containing the template name. Eg: {* threadBit *}"

compileStmt :: SmartyStmt -> CompM T.Text
compileStmt (SmartyText t) =
    return $ T.concat [ "s += ", T.pack $ show t, ";" ]
compileStmt (SmartyComment v) =
    return $ T.concat [ "/* ", v, "*/" ]
compileStmt (SmartyIf (If cases elsePart)) =
    do vCases <- mapM mkCase cases
       vElse <- case elsePart of
                  Just ep -> mkElse ep
                  Nothing -> return ""
       return $ T.concat [T.drop 2 $ T.concat vCases, vElse]
    where
      mkCase (test, stmts) =
          do vStmts <- mapM compileStmt stmts
             vExpr <- compileExpr test
             return $ T.concat [ "elseif (", vExpr, ") {", T.concat vStmts, "} "]
compileStmt (SmartyForeach (Foreach source key item body elsePart)) =
    do vElse <- case elsePart of
                  Just ep -> mkElse ep
                  Nothing -> return ""
       vSource <- compileExpr source
       gen <- liftIO $ newStdGen
       let varName = (T.pack . take 12) $ randomRs ('a', 'z') gen
           loopBody = T.concat [ "for (var ", varName, " in ", vSource, "{}"]
       return $ T.concat [ "if ("
                         , vSource, ".length > 0) {"
                         , loopBody
                         , "}"
                         , vElse
                         ]
compileStmt (SmartyPrint val dirs) =
    do vVal <- compileExpr val
       return $ T.concat [ "s += "
                         , T.concat (map mkDir dirs)
                         , vVal
                         , T.pack $ replicate (length dirs) ')'
                         , ";"
                         ]
    where
      mkDir dir =
          T.concat [ "__print", dir, "(" ]

mkElse stmts =
    do vStmts <- mapM compileStmt stmts
       return $ T.concat [ "else {", T.concat vStmts, "}"]

#define mkBinOp(_OPCHR_, _OPCONST_) compileExpr(ExprBin (_OPCONST_ a b)) = do { av <- compileExpr a; bv <- compileExpr b; \
                                                                                return $ T.concat [ av, _OPCHR_, bv ] }
compileExpr :: Expr -> CompM T.Text
compileExpr (ExprVar var) =
    renderVar var
    where
      renderVar (Variable name path idx prop) =
          do vIdx <- case idx of
                       Just a -> do v <- compileExpr a
                                    return (Just v)
                       Nothing -> return Nothing

             return $ T.concat [ "__readVar("
                               , "tplArgs,"
                               , T.pack $ show name
                               , ","
                               , "[", T.intercalate "," (map (T.pack . show) path), "],"
                               , fromMaybe "null" vIdx
                               , ","
                               , fromMaybe "null" (fmap (T.pack . show) prop)
                               , ")"
                               ]
compileExpr (ExprLit val) =
    return $ T.pack $ show val
compileExpr (ExprFun (FunctionCall name args)) =
    do argVals <- mapM mkArg args
       return $ T.concat [ name, "({", T.intercalate "," argVals, "})" ]
    where
      mkArg (k, v) =
          do vVal <- compileExpr v
             return $ T.concat [ T.pack $ show k
                               , ":"
                               , vVal
                               ]

compileExpr (ExprBin (BinNot a)) =
    do av <- compileExpr a
       return $ T.concat ["!", av]
mkBinOp("===", BinEq)
mkBinOp("&&", BinAnd)
mkBinOp("||", BinOr)
mkBinOp(">", BinLarger)
mkBinOp("<", BinSmaller)
mkBinOp(">=", BinLargerEq)
mkBinOp("<=", BinSmallerEq)
mkBinOp("+", BinPlus)
mkBinOp("-", BinMinus)
mkBinOp("*", BinMul)
mkBinOp("/", BinDiv)
