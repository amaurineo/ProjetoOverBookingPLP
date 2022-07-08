module Util where
import System.IO

lerEntradaString :: IO String
lerEntradaString = do
    x <- getLine
    return x