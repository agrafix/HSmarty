{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Text.HSmarty.Render.Engine
  ( ParamMap,
    mkParam,
    SmartyCtx,
    SmartyError (..),
    prepareTemplate,
    prepareTemplates,
    applyTemplate,
    applyTemplateFromJson,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Vector ((!?))
import qualified Data.Vector as V
import Network.HTTP.Base (urlEncode)
import System.FilePath
import System.FilePath.Glob
import Text.HSmarty.Parser.Smarty
import Text.HSmarty.Types

data TemplateVar = TemplateVar
  { tv_value :: A.Value,
    tv_props :: PropMap
  }
  deriving (Show, Eq)

-- | Maps template variables to template params
type ParamMap = HM.HashMap T.Text A.Value

type PropMap = HM.HashMap T.Text A.Value

mkParam :: A.ToJSON a => a -> A.Value
mkParam = A.toJSON

type EvalM m a = ExceptT SmartyError m a

newtype SmartyCtx = SmartyCtx
  { unSmartyCtx :: HM.HashMap FilePath Smarty
  }
  deriving (Show, Eq)

data Env = Env
  { e_var :: HM.HashMap T.Text TemplateVar,
    e_fun :: HM.HashMap T.Text ([SmartyStmt], [(T.Text, A.Value)]),
    e_ctx :: SmartyCtx
  }
  deriving (Show, Eq)

newtype SmartyError = SmartyError {unSmartyError :: T.Text}
  deriving (Show, Eq)

wrapSmartyCapture :: A.Value -> PropMap -> TemplateVar
wrapSmartyCapture x = TemplateVar (A.Object $ KM.singleton "capture" x)

updateSmartyCapture :: T.Text -> A.Value -> HM.HashMap T.Text TemplateVar -> HM.HashMap T.Text TemplateVar
updateSmartyCapture key value hm =
  let updatedVal =
        case HM.lookup "smarty" hm of
          Nothing -> wrapSmartyCapture (A.Object $ KM.singleton (AK.fromText key) value) mempty
          Just (TemplateVar existingCtx oldProps) ->
            case existingCtx of
              A.Object oldContext ->
                wrapSmartyCapture (A.Object $ KM.insert (AK.fromText key) value oldContext) oldProps
              _ -> error "Smarty capture context is always a map!"
   in HM.insert "smarty" updatedVal hm

rootMap :: HM.HashMap T.Text TemplateVar
rootMap = HM.singleton "smarty" (wrapSmartyCapture (A.Object mempty) mempty)

mkEnv :: ParamMap -> SmartyCtx -> Env
mkEnv pm ctx =
  Env
    { e_var = rootMap <> HM.map (\init' -> TemplateVar init' HM.empty) pm,
      e_fun = HM.empty,
      e_ctx = ctx
    }

-- | Parse and compile a template
prepareTemplate :: FilePath -> IO SmartyCtx
prepareTemplate fp =
  do
    ct <- T.readFile fp
    tpl <- HM.singleton fp <$> parseSmarty fp ct
    pure $ SmartyCtx tpl

-- | Parse and compiles templates matching a glob in a directiry
prepareTemplates :: String -> FilePath -> IO SmartyCtx
prepareTemplates pat dir =
  do
    files <- globDir1 (compile pat) dir
    let dirDropper =
          makeRelative dir
    ctx <-
      foldM
        ( \hm f ->
            do
              ct <- T.readFile f >>= parseSmarty f
              pure (HM.insert (dirDropper f) ct hm)
        )
        mempty
        files
    pure $ SmartyCtx ctx

-- | Fill a template with values and print it as Text
applyTemplateFromJson :: A.ToJSON a => FilePath -> SmartyCtx -> a -> Either SmartyError T.Text
applyTemplateFromJson a b c =
  case A.toJSON c of
    A.Object hm -> runIdentity $ runExceptT $ applyTemplate' a b $ HM.mapKeys AK.toText $ HM.fromList $ KM.toList hm
    x -> Left (SmartyError $ T.pack $ show x ++ " is not a json object, need an object at top level")

-- | Fill a template with values and print it as Text
applyTemplate :: FilePath -> SmartyCtx -> ParamMap -> Either SmartyError T.Text
applyTemplate a b c =
  runIdentity $ runExceptT $ applyTemplate' a b c

applyTemplate' :: Monad m => FilePath -> SmartyCtx -> ParamMap -> ExceptT SmartyError m T.Text
applyTemplate' template ctx mp =
  do
    getTpl <-
      case HM.lookup template (unSmartyCtx ctx) of
        Just ok -> pure ok
        Nothing ->
          throwError (SmartyError $ T.pack $ "Template " ++ template ++ " not compiled")
    evalTpl (mkEnv mp ctx) getTpl

txtPdHelper ::
  Monad m => Env -> Expr -> (T.Text -> T.Text) -> ExceptT SmartyError m Expr
txtPdHelper env expr go =
  do
    t <- exprToText env expr
    return $ ExprLit $ A.String $ go t

applyPrintDirective :: Monad m => Env -> Expr -> PrintDirective -> EvalM m Expr
applyPrintDirective env expr "json" =
  do
    evaled <- evalExpr env expr
    pure $ ExprLit $ A.String $ T.decodeUtf8 $ BSL.toStrict $ A.encode evaled
applyPrintDirective env expr "urlencode" =
  txtPdHelper env expr (T.pack . urlEncode . T.unpack)
applyPrintDirective env expr "nl2br" =
  txtPdHelper env expr (T.replace "\n" "<br />")
applyPrintDirective env expr "escape" =
  txtPdHelper env expr (T.pack . htmlEscape . T.unpack)
  where
    forbidden :: String
    forbidden = "<&\">'/"
    htmlEscape :: String -> String
    htmlEscape [] = []
    htmlEscape (x : xs) =
      if x `elem` forbidden
        then
          concat
            [ "&#" ++ show (ord x) ++ ";",
              htmlEscape xs
            ]
        else x : htmlEscape xs
applyPrintDirective _ _ pd =
  throwError $
    SmartyError $
      T.concat
        [ "Unknown print directive `",
          pd,
          "`"
        ]

evalTpl :: Monad m => Env -> Smarty -> EvalM m T.Text
evalTpl env (Smarty _ tpl) =
  snd <$> seqStmts env tpl

evalStmt :: Monad m => Env -> SmartyStmt -> EvalM m (Env, T.Text)
evalStmt env (SmartyScope sc) =
  do
    (_, body) <- seqStmts env (s_stmts sc)
    pure (env, body)
evalStmt env (SmartyFun fd) =
  do
    args <-
      forM (fd_defArgs fd) $ \(n, expr) ->
        do
          r <- evalExpr env expr
          pure (n, r)
    let fun =
          HM.insert (fd_name fd) (fd_body fd, args) (e_fun env)
    pure (env {e_fun = fun}, T.empty)
evalStmt env (SmartyCapture cap) =
  do
    (_, body) <- seqStmts env (c_stmts cap)
    let varBody = TemplateVar (A.String body) mempty
        capture =
          updateSmartyCapture (c_name cap) (A.String body)
        assignment =
          case c_assign cap of
            Nothing -> mempty
            Just x -> HM.insert x varBody
        eVars = capture . assignment $ e_var env
    pure (env {e_var = eVars}, T.empty)
evalStmt env (SmartyLet l) =
  do
    r <- evalExpr env (l_expr l)
    let eVars =
          HM.insert (l_name l) (TemplateVar r mempty) (e_var env)
    pure (env {e_var = eVars}, T.empty)
evalStmt env (SmartyText t) = return (env, t)
evalStmt env (SmartyComment _) = return (env, T.empty)
evalStmt env (SmartyPrint expr directives) =
  do
    e <- foldM (applyPrintDirective env) expr directives
    (,) <$> pure env <*> exprToText env e
evalStmt env (SmartyIf (If cases elseBody)) =
  do
    evaledCases <-
      mapM
        ( \(cond, body) ->
            do
              r <- evalExpr env cond
              (_, b) <- seqStmts env body
              case r of
                (A.Bool False) ->
                  return Nothing
                A.Null -> return Nothing
                (A.Array v) | null v -> return Nothing
                _ ->
                  return $ Just b
        )
        cases
    case catMaybes evaledCases of
      (x : _) ->
        return (env, x)
      _ ->
        case elseBody of
          Just elseB ->
            (,) <$> pure env <*> (snd <$> seqStmts env elseB)
          Nothing ->
            return (env, T.empty)
evalStmt env (SmartyForeach (Foreach source mKey val body elseBody)) =
  do
    evaledSource <- evalExpr env source
    (preparedSource, size) <- mkForeachInput evaledSource
    if size == 0
      then case elseBody of
        Just b -> (,) <$> pure env <*> (snd <$> seqStmts env b)
        Nothing -> return (env, T.empty)
      else do
        runs <- mapM (evalForeachBody env mKey val body) preparedSource
        return (env, T.concat runs)

seqStmts :: Monad m => Env -> [SmartyStmt] -> EvalM m (Env, T.Text)
seqStmts env stmt =
  do
    let go (ebase, tb) st =
          do
            (e, t) <- evalStmt ebase st
            pure (e, tb <> TLB.fromText t)
    (e', tb) <-
      foldM go (env, mempty) stmt
    return (e', TL.toStrict $ TLB.toLazyText tb)

exprToText :: Monad m => Env -> Expr -> EvalM m T.Text
exprToText env expr =
  do
    evaled <- evalExpr env expr
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

evalForeachBody ::
  Monad m =>
  Env ->
  Maybe T.Text ->
  T.Text ->
  [SmartyStmt] ->
  (A.Value, A.Value, PropMap) ->
  EvalM m T.Text
evalForeachBody envFull mKey item body (keyVal, itemVal, props) =
  let env = e_var envFull
      env' = HM.insert item (TemplateVar itemVal props) env
      env'' =
        case mKey of
          Just key -> HM.insert key (TemplateVar keyVal HM.empty) env'
          Nothing -> env'
   in snd <$> seqStmts (envFull {e_var = env''}) body

mkForeachInput :: Monad m => A.Value -> EvalM m ([(A.Value, A.Value, PropMap)], Int)
mkForeachInput (A.Array vec) =
  return $
    ( V.toList $
        V.imap
          ( \idx el ->
              ( A.Number (fromIntegral idx),
                el,
                mkForeachMap idx fSize
              )
          )
          vec,
      fSize
    )
  where
    fSize = V.length vec
mkForeachInput (A.Object hm) =
  let (_, input) =
        HM.foldlWithKey'
          ( \(idx, out) key el ->
              let newElem =
                    ( A.String $ AK.toText key,
                      el,
                      mkForeachMap idx hSize
                    )
               in (idx + 1, newElem : out)
          )
          (0, [])
          (HM.fromList $ KM.toList hm)
   in return (reverse input, hSize)
  where
    hSize = KM.size hm
mkForeachInput _ =
  throwError $ SmartyError "Tried to iterate over non traversable type."

mkForeachMap :: Int -> Int -> PropMap
mkForeachMap idx' size' =
  HM.fromList
    [ ("index", A.Number idx),
      ("iteration", A.Number $ 1 + idx),
      ("first", A.Bool $ idx == 0),
      ("last", A.Bool $ (idx + 1) == size),
      ("total", A.Number size)
    ]
  where
    size = fromIntegral size'
    idx = fromIntegral idx'

str :: Monad m => T.Text -> A.Value -> EvalM m T.Text
str _ (A.String x) = return x
str desc _ = throwError $ SmartyError $ T.concat ["`", desc, "` is not a string!"]

ifExists :: (Eq a, Show a, Monad m) => T.Text -> a -> [(a, A.Value)] -> (A.Value -> EvalM m b) -> EvalM m b
ifExists msg key env fun =
  case lookup key env of
    Just x -> fun x
    Nothing ->
      throwError $ SmartyError $ T.concat ["`", T.pack $ show key, "` is not given. ", msg]

lookupStr :: Monad m => T.Text -> T.Text -> [(T.Text, A.Value)] -> EvalM m T.Text
lookupStr funName key env =
  ifExists (T.concat ["Param for `", funName, "`"]) key env (str key)

evalFunCall :: Monad m => Env -> T.Text -> [(T.Text, Expr)] -> EvalM m A.Value
evalFunCall env "include" args =
  do
    evaledArgs <-
      mapM
        ( \(k, expr) ->
            do
              val <- evalExpr env expr
              return (k, val)
        )
        args
    filename <- lookupStr "include" "file" evaledArgs
    let otherArgs =
          filter
            ( \(k, _) ->
                not $ k `elem` ["include"]
            )
            evaledArgs
        asTplParams = HM.fromList $ map (\(k, v) -> (k, v)) otherArgs
    A.String <$> applyTemplate' (T.unpack filename) (e_ctx env) asTplParams
evalFunCall env fname args =
  case HM.lookup fname (e_fun env) of
    Just (fBody, fDefArgs) ->
      do
        localArgs <-
          forM args $ \(n, expr) ->
            do
              r <- evalExpr env expr
              pure (n, r)
        let myArgs =
              fmap (\v -> TemplateVar v mempty) $
                HM.fromList localArgs `HM.union` HM.fromList fDefArgs
            callEnv =
              myArgs `HM.union` e_var env
        (_, res) <- seqStmts (env {e_var = callEnv}) fBody
        return (A.String res)
    Nothing ->
      throwError $
        SmartyError $
          T.concat
            [ "Call to undefined function ",
              fname
            ]

evalExpr :: Monad m => Env -> Expr -> EvalM m A.Value
evalExpr _ (ExprLit v) = return v
evalExpr env (ExprBin op) =
  evalBinOp env op
evalExpr env (ExprFun funCall) =
  evalFunCall env (f_name funCall) (f_args funCall)
evalExpr env (ExprVar v) =
  case HM.lookup varName (e_var env) of
    Just tplVar ->
      case v of
        (Variable {v_prop = Just propReq}) ->
          case HM.lookup propReq (tv_props tplVar) of
            Just val -> return val
            Nothing ->
              throwError $
                SmartyError $
                  T.concat
                    [ "Property `",
                      propReq,
                      "` is not defined for variable `",
                      varName,
                      "`"
                    ]
        (Variable {v_path = path, v_index = mIdx}) ->
          let pathName =
                T.concat
                  [ varName,
                    if (length path > 0) then "." else T.empty,
                    T.intercalate "." path
                  ]
              idxWalk val =
                case mIdx of
                  Just eIdx ->
                    do
                      res <- evalExpr env eIdx
                      walkIndex pathName res val
                  Nothing ->
                    return val
           in do
                pathRes <- walkPath varName path $ tv_value tplVar
                idxWalk pathRes
    Nothing ->
      throwError $
        SmartyError $
          T.concat
            [ "Variable `",
              varName,
              "` is not defined"
            ]
  where
    varName = v_name v

walkIndex :: Monad m => T.Text -> A.Value -> A.Value -> EvalM m A.Value
walkIndex vname (A.Number idx) (A.Array arr) =
  case arr !? (fromJust $ toBoundedInteger idx) of
    Just val -> return val
    Nothing ->
      throwError $
        SmartyError $
          T.concat
            [ "Out of bounds. `",
              vname,
              "[",
              T.pack $ show idx,
              "]` not defined."
            ]
walkIndex vname idx _ =
  throwError $
    SmartyError $
      T.concat
        [ "Can't access `",
          T.pack $ show idx,
          "` in `",
          vname,
          "`. Index is not an integer or value not an array!"
        ]

walkPath :: Monad m => T.Text -> [T.Text] -> A.Value -> EvalM m A.Value
walkPath _ [] val = return val
walkPath vname (path : xs) (A.Object obj) =
  case KM.lookup (AK.fromText path) obj of
    Just val -> walkPath (T.concat [vname, ".", path]) xs val
    Nothing ->
      throwError $
        SmartyError $
          T.concat
            [ "Variable `",
              vname,
              "` doesn't have the key `",
              path,
              "`"
            ]
walkPath vname (path : _) _ =
  throwError $
    SmartyError $
      T.concat
        [ "Variable `",
          vname,
          "` is not a map! Can't lookup `",
          path,
          "`"
        ]

evalBinOp :: Monad m => Env -> BinOp -> EvalM m A.Value
evalBinOp env (BinEq a b) =
  boolResOp (\x y -> return $ x == y) (a, b) env
evalBinOp env (BinNot e) =
  do
    e' <- evalExpr env e
    case e' of
      A.Bool a ->
        return (A.Bool $ not a)
      _ ->
        throwError $ SmartyError "Tried to evaluate a NOT on a non boolean value"
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

boolOp :: Monad m => T.Text -> (Bool -> Bool -> Bool) -> (Expr, Expr) -> Env -> EvalM m A.Value
boolOp d op exprs env =
  boolResOp bOp exprs env
  where
    bOp (A.Bool a) (A.Bool b) =
      return $ a `op` b
    bOp _ _ = throwError $ SmartyError $ T.concat ["Tried ", d, "Op and on two non boolean values"]

numOp :: Monad m => T.Text -> (Scientific -> Scientific -> Bool) -> (Expr, Expr) -> Env -> EvalM m A.Value
numOp =
  numGenOp boolResOp

calcOp :: Monad m => T.Text -> (Scientific -> Scientific -> Scientific) -> (Expr, Expr) -> Env -> EvalM m A.Value
calcOp =
  numGenOp numResOp

numGenOp ::
  Monad m =>
  ( (A.Value -> A.Value -> EvalM m a) ->
    (Expr, Expr) ->
    Env ->
    EvalM m A.Value
  ) ->
  T.Text ->
  (Scientific -> Scientific -> a) ->
  (Expr, Expr) ->
  Env ->
  EvalM m A.Value
numGenOp fun d op exprs env =
  fun nOp exprs env
  where
    nOp (A.Number a) (A.Number b) =
      return $ a `op` b
    nOp _ _ = throwError $ SmartyError $ T.concat ["Tried ", d, "Op and on two non numeric values"]

numResOp ::
  Monad m =>
  (A.Value -> A.Value -> EvalM m Scientific) ->
  (Expr, Expr) ->
  Env ->
  EvalM m A.Value
numResOp fun (a, b) env =
  do
    a' <- evalExpr env a
    b' <- evalExpr env b
    A.Number <$> fun a' b'

boolResOp ::
  Monad m =>
  (A.Value -> A.Value -> EvalM m Bool) ->
  (Expr, Expr) ->
  Env ->
  EvalM m A.Value
boolResOp fun (a, b) env =
  do
    a' <- evalExpr env a
    b' <- evalExpr env b
    A.Bool <$> fun a' b'
