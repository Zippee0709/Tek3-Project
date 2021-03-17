module ErrorSpec where
import Text.Printf
import Text.Read

checkArgs :: [String] -> Int
checkArgs args
    | length args /= 1 = 84
    | otherwise = 0

---------------------------------------------------------------------
newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }

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

openParenthesis :: String -> Int -> Int
openParenthesis [] n = n
openParenthesis ('(':xs) n = openParenthesis xs (n + 1)
openParenthesis (x:xs) n = openParenthesis xs n

closeParenthesis :: String -> Int -> Int
closeParenthesis [] n = n
closeParenthesis (')':xs) n = closeParenthesis xs (n + 1)
closeParenthesis (x:xs) n = closeParenthesis xs n

operator :: String
operator = "+-*/^"

legal :: String -> String
legal s = s ++ ".()+-*/^ "

illegal :: String -> Bool
illegal s = case runParser (parseSome $ parseAnyChar $ legal ['0'..'9']) s of
    Just (a, []) -> case openParenthesis s 0 == closeParenthesis s 0 of
        True -> True
        False -> False
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

myRound :: (PrintfArg a, Floating a) => Int -> a -> Double
myRound n f = read (printf ("%0." ++ show n ++ "f") f) :: Double