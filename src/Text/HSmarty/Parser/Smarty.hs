{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Text.HSmarty.Parser.Smarty where

import Text.HSmarty.Parser.Util
import Text.HSmarty.Types

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char

import qualified Data.Aeson as A
import qualified Data.Attoparsec.Expr as E
import qualified Data.Text as T

parseSmarty :: Monad m => FilePath -> T.Text -> m Smarty
parseSmarty fp t =
    either fail mk $ parseOnly pRoot t
    where
      mk exprs =
          return $ Smarty fp exprs

pRoot :: Parser [SmartyStmt]
pRoot =
    (stripSpace $ many1 pStmt) <* endOfInput

pStmt :: Parser SmartyStmt
pStmt =
    SmartyComment <$> pComment <|>
    SmartyText <$> pLiteral <|>
    SmartyIf <$> pIf <|>
    SmartyForeach <$> pForeach <|>
    SmartyCapture <$> pCapture <|>
    SmartyScope <$> pScope <|>
    SmartyFun <$> pFunDef <|>
    braced (char '{') (char '}') (SmartyPrint <$> pExpr <*> many pPrintDirective) <|>
    SmartyText <$> (takeWhile1 (/='{'))

pPrintDirective :: Parser PrintDirective
pPrintDirective =
    char '|' *> pName

pExpr :: Parser Expr
pExpr =
    E.buildExpressionParser opTable pValExpr

pValExpr :: Parser Expr
pValExpr =
    braced (char '(') (char ')') pExpr <|>
    ExprVar <$> pVar <|>
    ExprLit <$> pLit <|>
    ExprFun <$> pFunCall

pLit :: Parser A.Value
pLit =
    A.String <$> stringP <|>
    A.Bool <$> boolP <|>
    A.Number <$> scientific

pVar :: Parser Variable
pVar =
    Variable <$> (char '$' *> pName) <*> many pVarPath <*> optional pVarIndex <*> optional pVarProp
    where
      pVarProp =
          char '@' *> pName
      pVarIndex =
          braced (char '[') (char ']') pExpr
      pVarPath =
          char '.' *> pName

pName :: Parser T.Text
pName =
    identP (\c -> isAlpha c || c == '_') (\c -> isAlphaNum c || c == '_')

pLiteral :: Parser T.Text
pLiteral =
    (pOpen "literal") *> (T.pack <$> manyTill anyChar (pClose "literal"))

pComment :: Parser T.Text
pComment =
    (string "{*") *> (T.pack <$> manyTill anyChar (string "*}"))

pFunCall :: Parser FunctionCall
pFunCall =
    FunctionCall <$> pName <*> many1 pArg
    where
      pArg =
          (,) <$> (space_ *> pName <* (stripSpace $ char '='))
              <*> pExpr

pOpen :: T.Text -> Parser T.Text
pOpen t =
    string $ T.concat [ "{", t, "}" ]

pOpenExpr :: T.Text -> Parser Expr
pOpenExpr t =
    (string (T.concat [ "{", t]) *> space_) *> pExpr <* char '}'

pClose :: T.Text -> Parser T.Text
pClose t =
    string $ T.concat [ "{/", t, "}" ]

pLet :: Parser Let
pLet =
    Let
    <$> (string "{$" *> pName <* char '=')
    <*> pExpr <* char '}'

pScope :: Parser Scope
pScope =
    Scope <$> (pOpen "scope" *> many pStmt <* pClose "scope")

pCapture :: Parser Capture
pCapture =
    Capture
    <$> ((string "{capture name=" *> space_) *> stringP <* char '}')
    <*> (many pStmt <* pClose "capture")

pFunDef :: Parser FunctionDef
pFunDef =
    FunctionDef
    <$> ((string "{function name=" *> space_) *> stringP)
    <*> (many pArg <* char '}')
    <*> (many pStmt <* pClose "function")
    where
      pArg =
          (,) <$> (space_ *> pName <* stripSpace (char '='))
              <*> pExpr

pIf :: Parser If
pIf =
    If <$> pBranches <*> optional (pOpen "else" *> many pStmt)
       <* pClose "if"
    where
      pBranch ty = (,) <$> pOpenExpr ty <*> many pStmt
      pBranches =
          (:) <$> pBranch "if" <*> (many $ pBranch "elseif")

pForeach :: Parser Foreach
pForeach =
    Foreach <$> ((string "{foreach" *> space_) *> pExpr <* (space_ <* (string "as") <* space_))
            <*> optional (char '$' *> pName <* (stripSpace $ string "=>"))
            <*> ((stripSpace (char '$' *> pName)) <* char '}')
            <*> many pStmt
            <*> optional (pOpen "foreachelse" *> many pStmt)
            <* pClose "foreach"

opTable :: [[E.Operator T.Text Expr]]
opTable =
    [ [ prefix (string "not" *> space_) $ arg1 BinNot
      , prefix (char '!') $ arg1 BinNot
      ]
    , [ sym "*" (arg2 BinMul) E.AssocLeft
      , sym "/" (arg2 BinDiv) E.AssocLeft
      ]
    , [ sym "+" (arg2 BinPlus) E.AssocLeft
      , sym "-" (arg2 BinMinus) E.AssocLeft
      ]
    , [ sym "<" (arg2 BinSmaller) E.AssocNone
      , sym ">" (arg2 BinLarger) E.AssocNone
      , sym "<=" (arg2 BinSmallerEq) E.AssocNone
      , sym ">=" (arg2 BinLargerEq) E.AssocNone
      ]
    , [ sym "==" (arg2 BinEq) E.AssocNone
      , sym "!=" (arg2 (\x y ->
                            BinNot $ ExprBin $ BinEq x y
                       )) E.AssocNone
      ]
    , [ wsym "and" (arg2 BinAnd) E.AssocLeft
      , wsym "or" (arg2 BinOr) E.AssocLeft
      , sym "&&" (arg2 BinAnd) E.AssocLeft
      , sym "||" (arg2 BinOr) E.AssocLeft
      ]
    ]
    where
      arg1 fun x = ExprBin $ fun x
      arg2 fun x y = ExprBin $ fun x y

      binary op fun assoc =
          E.Infix (fun <$ op <* optSpace_) assoc
      prefix op fun =
          E.Prefix (fun <$ op <* optSpace_)
      sym s =
          binary (stripSpace $ string s)
      wsym w =
          binary (between optSpace_ space_ $ string w)
