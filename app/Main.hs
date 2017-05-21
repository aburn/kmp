module Main where

import Lib

main :: IO ()
main = do
        pattern <- getLine
        string  <- getLine
        print (isSubstringOf pattern string)
