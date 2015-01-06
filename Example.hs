{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HSmarty

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

main :: IO ()
main =
   renderTemplate "test.tpl" $
   HM.fromList [ ( "title", mkParam ("SomeTitle" :: T.Text))
               , ( "list", mkParam (["a", "b"] :: [T.Text]))
               ]
