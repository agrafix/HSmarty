{-# LANGUAGE OverloadedStrings #-}
import Text.HSmarty.Compiler.JavaScript
import Text.HSmarty.Parser.Smarty

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Control.Monad.Trans.Error

main =
    do args <- getArgs
       case args of
         xxs@(_:_) ->
             do ct <- mapM (\file ->
                                do ct <- T.readFile file
                                   case parseSmarty file ct of
                                        Just val -> do comp <- runErrorT $ compile val
                                                       case comp of
                                                         Left err -> error $ T.unpack err
                                                         Right finalVal ->
                                                             return $ Just finalVal
                                        Nothing -> return Nothing) xxs
                putStrLn $ T.unpack $ T.intercalate "\n" $ catMaybes ct
         _ ->
             print "Usage: HSmarty file1 file2... fileN"
