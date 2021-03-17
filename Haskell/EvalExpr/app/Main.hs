module Main where
import Prelude
import System.Environment
import System.Exit
import Data.Char
import Data.List
import Data.Typeable()
import System.IO
import Text.Read
import Text.Printf
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }

printHelp :: IO()
printHelp = putStrLn "./funEvalExpr mathExpression\n" >>
  putStrLn "mathExpression\texpression to evaluate"

-- Error management
checkArgs :: [String] -> Int
checkArgs args
    | length args /= 1 = 84
    | otherwise = 0

errorGestion :: [String] -> IO()
errorGestion ["-h"] = printHelp
errorGestion ["--help"] = printHelp
errorGestion [] = exitWithErrorMsg "Error: Not enough argument" 84
errorGestion [x,xs] = exitWithErrorMsg "Error: Invalid argument" 84

exitWithErrorMsg :: String -> Int -> IO()
exitWithErrorMsg str e = putStrLn str >> exitWith (ExitFailure e)

openParenthesis :: String -> Int -> Int
openParenthesis [] n = n
openParenthesis ('(':xs) n = openParenthesis xs (n + 1)
openParenthesis (x:xs) n = openParenthesis xs n

closeParenthesis :: String -> Int -> Int
closeParenthesis [] n = n
closeParenthesis (')':xs) n = closeParenthesis xs (n + 1)
closeParenthesis (x:xs) n = closeParenthesis xs n

---------------------------------------------------------------------

parseChar :: Char -> Parser Char
parseChar char = Parser $ \s -> case s of
    (x:xs) | char == x -> Just (x, xs)
    _ -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser $ const Nothing
parseAnyChar (x:xs) = Parser $ \s ->
    case runParser (parseChar x) s of
        Just (a, b) -> Just (a, b)
        Nothing -> runParser (parseAnyChar xs) s

parseMany :: Parser a -> Parser [a]
parseMany func = Parser $ \s ->
    case runParser func s of
        Just (a, b) -> case runParser (parseMany func) b of
            Just (c, d) -> Just (a:c, d)
        Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome a = Parser $ \s ->
    case runParser (parseMany a) s of
        Just (a, b) -> case a of
            [] -> Nothing
            (x:_) -> Just (a, b)
        Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser $ \s ->
    case runParser (parseSome $ parseAnyChar ['0'..'9']) s of
        Just (num, str) -> Just (read num :: Int, str)
        Nothing -> Nothing

parseUDouble :: Parser Double
parseUDouble = Parser $ \s ->
    case runParser (parseSome $ parseAnyChar $ '.':['0'..'9']) s of
        Just (num, str) -> case readMaybe num :: Maybe Double of
            Just n -> Just (n, str)
            Nothing -> Nothing
        Nothing -> Nothing

parseNum :: Parser Double
parseNum = Parser $ \s -> case runParser (parseChar '-') s of
  Just (a,b) -> case runParser (parseSome $ parseAnyChar $ '.':['0'..'9']) b of
          Just (num, str) -> case readMaybe ('-':num) :: Maybe Double of
              Just n -> Just (n, str)
              Nothing -> Nothing
          Nothing -> Nothing
  Nothing -> runParser parseUDouble s

---------------------------------------------------------------------

data Expr
    = Val Double
    | Pow Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Add Expr Expr
    | Sub Expr Expr
    deriving  (Eq, Show)

pow :: Double -> Double -> Double
pow base 1 = base
pow base idx = base * pow base (idx - 1)

evalExpr :: Expr -> Either Double String
evalExpr (Div a b) = case evalExpr a of
    Left a -> case evalExpr b of
        Left 0 -> Right "Error : Operation : Division by 0"
        Left b -> Left (a / b)

evalExpr (Mul a b) = case evalExpr a of
    Left a -> case evalExpr b of
        Left b -> Left (a * b)

evalExpr (Pow a b) = case evalExpr a of
    Left a -> case evalExpr b of
        Left b | b <= 0 -> Right "Error : Operation : Pow by <= 0"
        Left b -> Left (a `pow` b)

evalExpr (Add a b) = case evalExpr a of
    Left a -> case evalExpr b of
        Left b -> Left (a + b)

evalExpr (Sub a b) = case evalExpr a of
    Left a -> case evalExpr b of
        Left b -> Left (a - b)

evalExpr (Val a) = Left a

getSign :: String -> Maybe (Expr -> Expr -> Expr, String)
getSign (x:xs) = case x of
    '+' -> Just (Add, xs)
    '-' -> Just (Sub, xs)
    '*' -> Just (Mul, xs)
    '/' -> Just (Div, xs)
    '^' -> Just (Pow, xs)
    _ -> Nothing

operator :: String
operator = "+-*/^"

legal :: String -> String
legal s = s ++ ".()+-*/^ "

illegal :: String -> Bool
illegal s = case runParser (parseSome $ parseAnyChar $ legal ['0'..'9']) s of
    Just (a, []) -> if openParenthesis s 0 == closeParenthesis s 0 then True else False
    Just (a, xs) -> False
    Nothing -> False

checkOperation :: String -> Bool -> Bool
checkOperation [] _ = True
checkOperation (' ':xs) state = checkOperation xs state
checkOperation (x:xs) state = case runParser (parseAnyChar operator) (x:xs) of
    Just (a, xs) -> if state == True then False else checkOperation xs True
    Nothing -> checkOperation xs False

checkNum :: String -> Bool -> Bool -> Bool
checkNum [] _ _ = True
checkNum (' ':xs) st sp = checkNum xs st True
checkNum (x:xs) st sp = case runParser (parseAnyChar ['0'..'9']) (x:xs) of
    Just (a, xs) -> if st == True && sp then False else checkNum xs True False
    Nothing -> checkNum xs False True

checkBlank :: String -> Bool -> Bool
checkBlank [] _ = True
checkBlank (' ':xs) st = if st then False else checkBlank xs True
checkBlank (x:xs) st = checkBlank xs False

checkInput :: String ->  Maybe String
checkInput [] = Nothing
checkInput s
    | a && b = Just s
    | otherwise = Nothing
    where a = illegal s && checkOperation s False
          b = checkNum s False False && checkBlank s False

transformSecond :: String -> Maybe (Expr, String)
transformSecond s = case runParser parseNum s of
    Just (a, xs) -> case xs of
        [] -> Just (Val a, [])
        (')':xs) -> Just (Val a, xs)
        _ -> case getSign xs of
            Just (sign, ys) -> case transformToExpr ys of
                Just (b, zs) -> Just (sign (Val a) b, zs)
                Nothing -> Nothing
            Nothing -> Nothing
    Nothing -> Nothing

transformToExpr :: String -> Maybe (Expr, String)
transformToExpr s = case runParser (parseChar '(') s of
    Just (a, xs) -> case transformToExpr xs of
        Just (a, xs) -> case xs of
            [] -> Just (a, [])
            _ -> case getSign xs of
                Just (sign, ys) -> case transformToExpr ys of
                    Just (b, zs) -> Just (sign a b, zs)
                    Nothing -> Nothing
                Nothing -> Nothing
        Nothing -> transformSecond s
    Nothing -> transformSecond s

printResult :: Double -> IO ()
printResult res = putStrLn $ printf "%.2f" res

-- myRound :: (PrintfArg a, Floating a) => Int -> a -> Double
-- myRound n f = read (printf ("%0." ++ show n ++ "f") f) :: Double

start :: String -> IO ()
start list = case checkInput list of
    Just list -> case transformToExpr $ concat $ words list of
        Just (n, _) -> case evalExpr n of
            Left res -> printResult res
            Right err -> exitWithErrorMsg err 84
        Nothing -> exitWithErrorMsg "Error: Invalid Syntax" 84
    Nothing -> exitWithErrorMsg "Error: Invalid Parsing" 84

main :: IO ()
main = do
    list <- getArgs
    case checkArgs list of
        84 -> errorGestion list
        _ -> start $ head list