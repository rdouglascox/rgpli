module Main where

import Lib
import Wffs

main :: IO ()
main = do
       out <- rgpli
       putStrLn out
