{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Text.HSmarty.Render.EngineTest where

import Paths_HSmarty
import Test.Framework
import Text.HSmarty
import qualified Data.Text as T
import qualified Data.Text.IO as T

readDataFile :: String -> IO T.Text
readDataFile name =
    do fp <- getDataFileName name
       T.readFile fp

engineTest :: FilePath -> FilePath -> IO ()
engineTest inputFile outputFile =
    do testInputFile <- getDataFileName inputFile
       expectedOutput <- readDataFile outputFile
       ctx <- prepareTemplate testInputFile
       case applyTemplate testInputFile ctx mempty of
           Left errMsg -> fail (show errMsg)
           Right ok -> assertEqual ok expectedOutput

test_capture :: IO ()
test_capture =
    engineTest "testCapture.tpl" "testCapture.txt"