{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Text.HSmarty.Parser.SmartyTest
import {-@ HTF_TESTS @-} Text.HSmarty.Render.EngineTest

main :: IO ()
main = htfMain htf_importedTests
