{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Text.HSmarty.Render.Engine
    ( renderTemplate, mkParam, TemplateParam )
where

import Text.HSmarty.Types
import Text.HSmarty.Parser.Smarty

import Control.Applicative
import Control.Monad.Error
import Data.Attoparsec.Text (Number(..))
import Data.Char (ord)
import Data.Maybe
import Data.Vector ((!?))
import Network.HTTP.Base (urlEncode)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

newtype TemplateParam
      = TemplateParam { unTemplateParam :: A.Value }
        deriving (Show, Eq)

data TemplateVar
   = TemplateVar
   { tv_value :: A.Value
   , tv_props :: PropMap
   }
   deriving (Show, Eq)

type Env = HM.HashMap T.Text TemplateVar
type ParamMap = HM.HashMap T.Text TemplateParam
type PropMap = HM.HashMap T.Text A.Value

type EvalM a = ErrorT T.Text IO a

instance Error T.Text where
    strMsg = T.pack

mkParam :: A.ToJSON a => a -> TemplateParam
mkParam = TemplateParam . A.toJSON

mkEnv :: ParamMap -> Env
mkEnv =
    HM.map (\init -> TemplateVar (unTemplateParam init) HM.empty)

renderTemplate :: FilePath -> ParamMap -> IO (Either T.Text T.Text)
renderTemplate fp mp =
    do ct <- T.readFile fp
       tpl <- parseSmarty fp ct
       runErrorT $ evalTpl (mkEnv mp) tpl

applyPrintDirective :: T.Text -> PrintDirective -> EvalM T.Text
applyPrintDirective t "urlencode" =
    return $ T.pack $ urlEncode $ T.unpack t
applyPrintDirective t "nl2br" =
    return $ T.replace "\n" "<br />" t
applyPrintDirective t "escape" =
    return $ T.pack $ htmlEscape $ T.unpack t
    where
      forbidden = "<&\">'/"
      htmlEscape :: String -> String
      htmlEscape [] = []
      htmlEscape (x:xs) =
          if x `elem` forbidden
          then concat [ "&#" ++ show (ord x) ++ ";"
                      , htmlEscape xs
                      ]
          else x : htmlEscape xs
applyPrintDirective _ pd =
    throwError $ T.concat [ "Unknown print directive `"
                          , pd
                          , "`"
                          ]

evalTpl :: Env -> Smarty -> EvalM T.Text
evalTpl env (Smarty filename tpl) =
    evalBody env tpl

evalStmt :: Env -> SmartyStmt -> EvalM T.Text
evalStmt _ (SmartyText t) = return t
evalStmt _ (SmartyComment _) = return T.empty
evalStmt env (SmartyPrint expr directives) =
    do t <- exprToText env expr
       foldM applyPrintDirective t directives
evalStmt env (SmartyIf (If cases elseBody)) =
    do evaledCases <- mapM (\(cond, body) ->
                                do r <- evalExpr env cond
                                   b <- evalBody env body
                                   case r of
                                     (A.Bool True) ->
                                         return $ Just b
                                     _ ->
                                         return Nothing
                           ) cases
       case catMaybes evaledCases of
         (x:_) ->
             return x
         _ ->
             case elseBody of
               Just elseB ->
                   evalBody env elseB
               Nothing ->
                   return T.empty

evalStmt env (SmartyForeach (Foreach source mKey val body elseBody)) =
    do evaledSource <- evalExpr env source
       (preparedSource, size) <- mkForeachInput evaledSource
       if size == 0
       then case elseBody of
              Just b -> evalBody env b
              Nothing -> return T.empty
       else do runs <- mapM (evalForeachBody env mKey val body) preparedSource
               return $ T.concat runs

evalBody :: Env -> [SmartyStmt] -> EvalM T.Text
evalBody env stmt =
    do b <- mapM (evalStmt env) stmt
       return $ T.concat b

exprToText :: Env -> Expr -> EvalM T.Text
exprToText env expr =
    do evaled <- evalExpr env expr
       case evaled of
         A.String t -> return t
         A.Number n -> return $ T.pack $ show n
         A.Null -> return "null"
         A.Bool b ->
             return (if b then "true" else "false")
         A.Object o ->
             return $ T.pack $ show o
         A.Array a ->
             return $ T.pack $ show a

evalForeachBody :: Env -> Maybe T.Text -> T.Text -> [ SmartyStmt ] -> ( A.Value, A.Value, PropMap ) -> EvalM T.Text
evalForeachBody env mKey item body (keyVal, itemVal, props) =
    let env' = HM.insert item (TemplateVar itemVal props) env
        env'' =
            case mKey of
              Just key -> HM.insert key (TemplateVar keyVal HM.empty) env'
              Nothing -> env'
    in evalBody env'' body


mkForeachInput :: A.Value -> EvalM ( [ ( A.Value, A.Value, PropMap ) ], Int)
mkForeachInput (A.Array vec) =
    return $ ( V.toList $ V.imap (\idx elem ->
                                      ( A.Number (I $ fromIntegral idx)
                                      , elem
                                      , mkForeachMap idx fSize
                                      )
                                 ) vec
             , fSize
             )
    where
      fSize = V.length vec
mkForeachInput (A.Object hm) =
    let (_, input) =
            HM.foldlWithKey' (\(idx, out) key elem ->
                                  let newElem = ( A.String key
                                                , elem
                                                , mkForeachMap idx hSize
                                                )
                                  in (idx+1, newElem : out)
                             ) (0, []) hm
    in return $ (reverse input, hSize)
    where
      hSize = HM.size hm
mkForeachInput _ =
    throwError "Tried to iterate over non traversable type."

mkForeachMap :: Int -> Int -> PropMap
mkForeachMap idx' size' =
    HM.fromList [ ("index", A.Number (I idx))
                , ("iteration", A.Number (I $ 1 + idx))
                , ("first", A.Bool $ idx == 0)
                , ("last", A.Bool $ (idx+1) == size)
                , ("total", A.Number (I size))
                ]
    where
      size = fromIntegral size'
      idx = fromIntegral idx'

str :: T.Text -> A.Value -> EvalM T.Text
str _ (A.String x) = return x
str desc _ = throwError $ T.concat [ "`", desc, "` is not a string!" ]

int :: T.Text -> A.Value -> EvalM Int
int _ (A.Number (I x)) = return (fromIntegral x)
int desc _ = throwError $ T.concat [ "`", desc, "` is not an integer!" ]

dbl :: T.Text -> A.Value -> EvalM Double
dbl _ (A.Number (D x)) = return x
dbl desc _ = throwError $ T.concat [ "`", desc, "` is not a double!" ]

ifExists :: (Eq a, Show a) => T.Text -> a -> [(a, A.Value)] -> (A.Value -> EvalM b) -> EvalM b
ifExists msg key env fun =
    case lookup key env of
      Just x -> fun x
      Nothing ->
          throwError $ T.concat [ "`", T.pack $ show key, "` is not given. ", msg]

lookupStr :: T.Text -> T.Text -> [(T.Text, A.Value)] -> EvalM T.Text
lookupStr funName key env =
    ifExists (T.concat ["Param for `", funName, "`"]) key env (str key)

evalFunCall :: Env -> T.Text -> [ (T.Text, Expr) ] -> EvalM A.Value
evalFunCall env "include" args =
    do evaledArgs <- mapM (\(k, expr) ->
                               do val <- evalExpr env expr
                                  return (k, val)
                          ) args
       filename <- lookupStr "include" "file" evaledArgs
       let otherArgs = filter (\arg@(k, _) ->
                                   not $ k `elem` [ "include" ]
                              ) evaledArgs
           asTplParams = HM.fromList $ map (\(k, v) -> (k, TemplateParam v)) otherArgs
       content <- liftIO $ renderTemplate (T.unpack filename) asTplParams
       case content of
         Right c ->
             return $ A.String c
         Left e ->
             throwError $ T.concat ["Include failed. Error: ", e]
evalFunCall env fname _ =
    throwError $ T.concat [ "Call to undefined function "
                          , fname
                          ]


evalExpr :: Env -> Expr -> EvalM A.Value
evalExpr _ (ExprLit v) = return v
evalExpr env (ExprBin op) =
    evalBinOp env op
evalExpr env (ExprFun funCall) =
    evalFunCall env (f_name funCall) (f_args funCall)
evalExpr env (ExprVar v) =
    case HM.lookup varName env of
      Just tplVar ->
          case v of
            (Variable { v_prop = Just propReq }) ->
                case HM.lookup propReq (tv_props tplVar) of
                  Just val -> return val
                  Nothing -> throwError $ T.concat [ "Property `"
                                                   , propReq
                                                   , "` is not defined for variable `"
                                                   , varName
                                                   , "`"
                                                   ]
            (Variable { v_path = path, v_index = mIdx }) ->
                let pathName = T.concat [ varName
                                        , if (length path > 0) then "." else T.empty
                                        , T.intercalate "." path
                                        ]
                    idxWalk val =
                        case mIdx of
                          Just eIdx ->
                              do res <- evalExpr env eIdx
                                 walkIndex pathName res val
                          Nothing ->
                              return val

                in do pathRes <- walkPath varName path $ tv_value tplVar
                      idxWalk pathRes


      Nothing ->
          throwError $ T.concat [ "Variable `"
                                , varName
                                , "` is not defined"
                                ]
    where
      varName = v_name v

walkIndex :: T.Text -> A.Value -> A.Value -> EvalM A.Value
walkIndex vname (A.Number (I idx)) (A.Array arr) =
    case arr !? (fromIntegral idx) of
      Just val -> return val
      Nothing ->
          throwError $ T.concat [ "Out of bounds. `"
                                , vname
                                , "["
                                , T.pack $ show idx
                                , "]` not defined."
                                ]
walkIndex vname idx _ =
    throwError $ T.concat [ "Can't access `"
                          , T.pack $ show idx
                          , "` in `"
                          , vname
                          , "`. Index is not an integer or value not an array!"
                          ]

walkPath :: T.Text -> [T.Text] -> A.Value -> EvalM A.Value
walkPath vname [] val = return val
walkPath vname (path:xs) (A.Object obj) =
    case HM.lookup path obj of
      Just val -> walkPath (T.concat [vname, ".", path]) xs val
      Nothing ->
          throwError $ T.concat [ "Variable `"
                                , vname
                                , "` doesn't have the key `"
                                , path
                                , "`"
                                ]
walkPath vname (path:xs) _ =
    throwError $ T.concat [ "Variable `"
                          , vname
                          , "` is not a map! Can't lookup `"
                          , path
                          , "`"
                          ]

evalBinOp :: Env -> BinOp -> EvalM A.Value
evalBinOp env (BinEq a b) =
    boolResOp (\x y -> return $ x == y) (a, b) env
evalBinOp env (BinNot e) =
    do e' <- evalExpr env e
       case e' of
         A.Bool a ->
             return (A.Bool $ not a)
         _ ->
             throwError "Tried to evaluate a NOT on a non boolean value"
evalBinOp env (BinOr x y) =
    boolOp "Or" (||) (x, y) env
evalBinOp env (BinAnd x y) =
    boolOp "And" (&&) (x, y) env
evalBinOp env (BinLarger x y) =
    numOp "Larger" (>) (x, y) env
evalBinOp env (BinLargerEq x y) =
    numOp "LargerEq" (>=) (x, y) env
evalBinOp env (BinSmaller x y) =
    numOp "Smaller" (<) (x, y) env
evalBinOp env (BinSmallerEq x y) =
    numOp "SmallerEq" (<=) (x, y) env
evalBinOp env (BinPlus x y) =
    calcOp "Plus" (+) (x, y) env
evalBinOp env (BinMinus x y) =
    calcOp "Minus" (-) (x, y) env
evalBinOp env (BinMul x y) =
    calcOp "Mul" (*) (x, y) env
evalBinOp env (BinDiv x y) =
    calcOp "Div" (/) (x, y) env


boolOp :: T.Text -> (Bool -> Bool -> Bool) -> (Expr, Expr) -> Env -> EvalM A.Value
boolOp d op exprs env =
    boolResOp bOp exprs env
    where
      bOp (A.Bool a) (A.Bool b) =
          return $ a `op` b
      bOp _ _ = throwError $ T.concat [ "Tried ", d, "Op and on two non boolean values" ]

numOp :: T.Text -> (Number -> Number -> Bool) -> (Expr, Expr) -> Env -> EvalM A.Value
numOp =
    numGenOp boolResOp

calcOp :: T.Text -> (Number -> Number -> Number) -> (Expr, Expr) -> Env -> EvalM A.Value
calcOp =
    numGenOp numResOp

numGenOp :: ((A.Value -> A.Value -> EvalM a)
                 -> (Expr, Expr) -> Env -> EvalM A.Value)
         -> T.Text -> (Number -> Number -> a) -> (Expr, Expr) -> Env -> EvalM A.Value
numGenOp fun d op exprs env =
    fun nOp exprs env
    where
      nOp (A.Number a) (A.Number b) =
          return $ a `op` b
      nOp _ _ = throwError $ T.concat [ "Tried ", d, "Op and on two non numeric values" ]

numResOp :: (A.Value -> A.Value -> EvalM Number)
       -> (Expr, Expr) -> Env -> EvalM A.Value
numResOp fun (a, b) env =
    do a' <- evalExpr env a
       b' <- evalExpr env b
       A.Number <$> fun a' b'

boolResOp :: (A.Value -> A.Value -> EvalM Bool)
       -> (Expr, Expr) -> Env -> EvalM A.Value
boolResOp fun (a, b) env =
    do a' <- evalExpr env a
       b' <- evalExpr env b
       A.Bool <$> fun a' b'
