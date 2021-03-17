module Main where
import System.Environment
import ErrorManagement
import Parser

main :: IO ()
main = do
    argv <- getArgs
    if checkArgv argv then runHAL argv else errorGestion argv