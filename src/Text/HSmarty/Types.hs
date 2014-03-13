{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.HSmarty.Types where

import qualified Data.Text as T
import qualified Data.Aeson as A

data Smarty
   = Smarty
   { s_name :: FilePath
   , s_template :: [ SmartyStmt ]
   } deriving (Eq, Show)

type PrintDirective = T.Text

data SmartyStmt
   = SmartyText T.Text
   | SmartyComment T.Text
   | SmartyIf If
   | SmartyForeach Foreach
   | SmartyPrint Expr [ PrintDirective ]
   deriving (Eq, Show)

data Expr
   = ExprVar Variable
   | ExprLit A.Value
   | ExprFun FunctionCall
   | ExprBin BinOp
   deriving (Eq, Show)

data Variable
   = Variable
   { v_name :: T.Text
   , v_path :: [T.Text]
   , v_index :: Maybe Expr
   , v_prop :: Maybe T.Text
   }
   deriving (Eq, Show)

data FunctionCall
   = FunctionCall
   { f_name :: T.Text
   , f_args :: [ (T.Text, Expr) ]
   }
   deriving (Eq, Show)

data BinOp
   = BinEq Expr Expr
   | BinNot Expr
   | BinAnd Expr Expr
   | BinOr Expr Expr
   | BinLarger Expr Expr
   | BinSmaller Expr Expr
   | BinLargerEq Expr Expr
   | BinSmallerEq Expr Expr
   | BinPlus Expr Expr
   | BinMinus Expr Expr
   | BinMul Expr Expr
   | BinDiv Expr Expr
   deriving (Eq, Show)

data If
   = If
   { if_cases :: [ (Expr, [SmartyStmt]) ]
   , if_else :: Maybe [SmartyStmt]
   }
   deriving (Eq, Show)

data Foreach
   = Foreach
   { f_source :: Expr
   , f_key :: Maybe T.Text
   , f_item :: T.Text
   , f_body :: [SmartyStmt]
   , f_else :: Maybe [SmartyStmt]
   }
   deriving (Eq, Show)
