{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Text.HSmarty.Parser.SmartyTest where

import Data.Attoparsec.Text
import Paths_HSmarty
import Test.Framework
import Text.HSmarty.Parser.Smarty
import Text.HSmarty.Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

parserTest :: forall b. (Eq b, Show b)
           => Parser b -> T.Text -> b -> IO ()
parserTest parser input expected =
    either fail comp $ parseOnly parser input
    where
      comp x =
          assertEqual x expected

readDataFile :: String -> IO T.Text
readDataFile name =
    do fp <- getDataFileName name
       T.readFile fp

test_complex :: IO ()
test_complex =
    do testData <- readDataFile "test.tpl"
       case parseOnly pRoot testData of
         Left errMsg ->
             fail errMsg
         Right _ ->
             return ()


test_literalParser :: IO ()
test_literalParser =
    parserTest pLiteral "{literal}abc{/literal}" "abc"

test_commentParser :: IO ()
test_commentParser =
    parserTest pComment "{* some comment *}" " some comment "

test_varParser :: IO ()
test_varParser =
    do parserTest pVar "$hallo.sub@prop" (Variable "hallo" ["sub"] Nothing (Just "prop"))
       parserTest pVar "$hallo.foo_bar" (Variable "hallo" ["foo_bar"] Nothing Nothing)

test_capture :: IO ()
test_capture =
    do parserTest pCapture "{capture name='foo'}Bye{/capture}" expect1
       parserTest pCapture "{capture name='foo' assign=bar}Bye{/capture}" expect2
    where
      expect1 = Capture "foo" Nothing [SmartyText "Bye"]
      expect2 = Capture "foo" (Just "bar") [SmartyText "Bye"]

test_if :: IO ()
test_if =
    do parserTest pIf "{if $var@last}Bye{/if}" expect
       parserTest pIf "{if ($var@last)}Bye{/if}" expect
    where
      expect =
          If
          { if_cases =
                  [( ExprVar (Variable "var" [] Nothing (Just "last"))
                   , [SmartyText "Bye"]
                   )
                  ]
          , if_else = Nothing
          }

test_rootParser :: IO ()
test_rootParser =
    parserTest pRoot "{if true}{include file='hallo.tpl' var1=23}{else}Nothing{/if}" expect
    where
      expect = [SmartyIf
                (If{if_cases =
                        [(ExprLit (A.Bool True),
                                      [SmartyPrint
                                       (ExprFun
                                        (FunctionCall{f_name = "include",
                                                      f_args =
                                                          [("file", ExprLit (A.String "hallo.tpl")),
                                                           ("var1", ExprLit (A.Number 23))]}))
                                       []])],
                        if_else = Just [SmartyText "Nothing"]})]
