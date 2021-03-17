module ErrorManagement (checkArgv, errorGestion, exitWithErrorMsg) where
import System.IO
import System.Exit

printHelp :: IO()
printHelp = putStrLn "./hal [file.acm]" >> putStrLn "\t[file.acm]\tList of file contains scheme function"

exitWithErrorMsg :: String -> Int -> IO()
exitWithErrorMsg str e = putStrLn str >> exitWith (ExitFailure e)

-- checkFileExist :: [String] -> Maybe IO ()
-- checkFileExist [] = True
-- checkFileExist (x:xs) = do 
--     res <- doesFileExist x
--     if res then checkFileExist xs else exitWithErrorMsg "Error: Invalid file" 84

checkArgv :: [String] -> Bool
checkArgv ["-h"] = False
checkArgv ["--help"] = False
checkArgv [] = False
checkArgv argv = True


errorGestion :: [String] -> IO ()
errorGestion ["-h"] = printHelp
errorGestion ["--help"] = printHelp
errorGestion [] = exitWithErrorMsg "Error: Not enough argument" 84
-- errorGestion argv = if not (checkFileExist argv) then exitWithErrorMsg "Error: Not enough argument" 84 else return