module Parser (
    runHAL,
    evalFile,
    readAllFile,
    intoExpr,
    evalExpr,
) where

import Text.Read
import Data.Char
import Control.Applicative
import System.IO
import GHC.IO.Exception
import Control.Exception
import Control.Monad
import ErrorManagement

    
runHAL :: [String] -> IO ()
runHAL av = readAllFile av

delSpace :: String -> String
delSpace file = unwords $ words $ file

readAllFile :: [String] -> IO ()
readAllFile (x:xs) = do
    myReadFile x `catch` showException
    file <- readFile x
    putStrLn $ evalFile (delSpace file)

showException :: IOException -> IO ()
showException e = exitWithErrorMsg (("Error : ") ++ (ioe_description e)) 84    

myReadFile :: String -> IO ()
myReadFile file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    hClose handle

--- A Changer
evalFile :: String -> String
evalFile s = show $ evalExpr $ intoExpr s

intoExpr :: String -> Expr
intoExpr s = case runParser transformToExpr s of
    Just (res, "") -> res
    Nothing -> Error "when intoExpr !"

----------------------------------------------------------------
-----------------------------AST--------------------------------
----------------------------------------------------------------

data Expr = NIL
    | Atom String
    | List [Expr]
    | Cons [Expr] Expr
    | Num Integer
    | Variable String
    | Boolean Bool
    | Error String
    -- deriving (Show)

instance Show Expr where show = showExpr

----------------------------------------------------------------
--------------------------MY PARSEC-----------------------------
----------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Just (x, xs) -> Just (f x, xs)
        Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    Parser fa <*> fb = Parser $ \s -> case fa s of
        Just (x, xs) -> case runParser fb xs of
            Just (y, ys) -> Just (x y, ys)
            Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser a) <|> b = Parser $ \s -> a s <|> runParser b s

parseChar :: Char -> Parser Char
parseChar char = Parser $ \s -> case s of
    (x:xs) | char == x -> Just (x, xs)
    _ -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr fa fb = fa <|> fb

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd fa fb = ((,) <$> fa) <*> fb

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func fa fb = (func <$> fa) <*> fb

parseAnyChar :: String -> Parser Char
parseAnyChar [] = empty
parseAnyChar (x:xs) = parseChar x <|> parseAnyChar xs

parseWord :: String -> Parser String
parseWord [] = pure []
parseWord (x:xs) = ((:) <$> parseChar x <*> parseWord xs) <|> empty

parseMany :: Parser a -> Parser [a]
parseMany fa = ((:) <$> fa <*> parseMany fa) <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome fa = (:) <$> fa <*> parseMany fa

parseUDouble :: Parser String
parseUDouble = parseSome (parseAnyChar $ '.':['0'..'9'])

parseDouble :: Parser String
parseDouble = parseSome (parseAnyChar $ '-':'.':['0'..'9'])

parseNum :: Parser String
parseNum = parseDouble <|> parseUDouble

parseTuple :: Parser a -> Parser (a, a)
parseTuple func = parseAndWith (\x y -> (x, y)) parseLeft parseRight
    where parseLeft = parseChar '(' *> func <* parseChar ','
          parseRight = func <* parseChar ')'

allCharacter :: String
allCharacter = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_!?-+*/%<>#"

----------------------------------------------------------------
----------------------------PARSING-----------------------------
----------------------------------------------------------------

transformToExpr :: Parser Expr
transformToExpr = mySpace <|> isList <|> isQuote <|> 
    isSymbole <|> isNum <|> isAtom

mySpace :: Parser Expr
mySpace = parseChar ' ' *> transformToExpr

isSymbole :: Parser Expr
isSymbole = addSign <|> subSign <|> mulSign <|> divSign <|> modSign
    where addSign = parseChar '+' *> pure (Atom "+") <* parseChar ' '
          subSign = parseChar '-' *> pure (Atom "-") <* parseChar ' '
          mulSign = parseChar '*' *> pure (Atom "*") <* parseChar ' '
          divSign = parseWord "div" *> pure (Atom "div") <* parseChar ' '
          modSign = parseWord "mod" *> pure (Atom "mod") <* parseChar ' '

isAtom :: Parser Expr
isAtom = isBoolTrue <|> isBoolFalse <|> isOther
    where isBoolTrue = parseWord "#t" *> pure (Boolean True)
          isBoolFalse = parseWord "#f" *> pure (Boolean False)
          isOther = (Atom <$> (parseSome $ parseAnyChar allCharacter))

isNum :: Parser Expr
isNum = Num <$> (read <$> parseNum)

exprToList :: Parser Expr -> Parser [Expr]
exprToList fa = ((\x -> [x]) <$> fa)

mySepBy :: Parser Expr -> Parser [Expr]
mySepBy fa = parseAndWith (\x y -> x ++ y) (exprToList fa) (mySepBy fa) 
    <|> exprToList fa

-- isCons :: Parser Expr
-- isCons = parseWord "cons" *> pure (parseAndWith (\x y -> (Cons x y)) transformToExpr transformToExpr)

isList :: Parser Expr
isList = parseChar '(' *> (List <$> mySepBy transformToExpr) <* parseChar ')'

isQuote :: Parser Expr
isQuote = parseWord "'()" *> pure (List [Atom "quote", NIL]) <|>
    parseChar '\'' *> ((\x -> (List [Atom "quote", x])) 
    <$> transformToExpr)

----------------------------------------------------------------
--------------------------EVALUATION----------------------------
----------------------------------------------------------------

unwordsList :: [Expr] -> String
unwordsList = unwords . map showExpr

showExpr :: Expr -> String
showExpr (NIL) = ""
showExpr (Atom name) = name
showExpr (Num value) = show value
showExpr (Boolean True) = "#t"
showExpr (Boolean False) = "#f"
showExpr (List contents) = "(" ++ unwordsList contents ++ ")"
showExpr (Cons a NIL) = "(" ++ unwordsList a ++ ")"
showExpr (Cons a b) = "(" ++ (unwordsList a) ++ " . " ++ (showExpr b) ++ ")"

evalExpr :: Expr -> Expr
evalExpr (Num value) = Num value
evalExpr (Boolean s) = Boolean s
evalExpr (List (Atom "quote": [])) = Atom "()"
evalExpr (List [Atom "quote", value]) = value
evalExpr (List (Atom func : av)) = apply func $ map evalExpr av

apply :: String -> [Expr] -> Expr
apply func av = maybe (Boolean False) ($ av) $ lookup func primitives

primitives :: [(String, [Expr] -> Expr)]
primitives = [("+", numericBinop (+)), ("-", numericBinop (-)),
              ("*", numericBinop (*)), ("div", numericBinop div),
              ("mod", numericBinop (mod)), ("eq?", numericCompare (==)), 
              (">", numericCompare (>)), ("<", numericCompare (<)),
              ("car", myCar), ("cdr", myCdr), 
              ("cons", myCons), ("list", myList)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericBinop op params = Num $ foldl1 op (map unPackNum params)

numericCompare :: (Integer -> Integer -> Bool) -> [Expr] -> Expr
numericCompare op params = Boolean (a `op` b)
    where a = unPackNum $ params !! 0
          b = unPackNum $ params !! 1

unPackNum :: Expr -> Integer
unPackNum (Num n) = n
unPackNum (List [n]) = unPackNum n
unPackNum _ = 0

myCar :: [Expr] -> Expr
myCar [List (x:xs)] = x
myCar [Cons (x:xs) _] = x

myCdr :: [Expr] -> Expr
myCdr [List (x:xs)] = List xs
myCdr [Cons [_] x] = x
myCdr [Cons (_ : xs) x] = Cons xs x

myCons :: [Expr] -> Expr
myCons [x, List []] = List [x]
myCons [x, List xs] = List (x:xs)
myCons [x, Cons xs ys] = Cons (x:xs) ys
myCons [x, y] = Cons [x] y

myList :: [Expr] -> Expr
myList x = Cons x NIL

----------------------------------------------------------------
------------------------ERROR EXCEPTION-------------------------
----------------------------------------------------------------

-- data LispError = NumArgs Integer [Expr]
--     | TypeMismatch String Expr
--     | BadSpecialForm String Expr
--     | NotFunction String String
--     | UnboundVar String String
--     | Default String

-- showError :: LispError -> String
-- showError (UnboundVar message varname)  = message ++ ": " ++ varname
-- showError (BadSpecialForm message form) = message ++ ": " ++ show form
-- showError (NotFunction message func)    = message ++ ": " ++ show func
-- showError (NumArgs expected found)      = 
--     "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
-- showError (TypeMismatch expected found) = 
--     "Invalid type: expected " ++ expected ++ ", found " ++ show found

-- instance Show LispError where show = showError

-- trapError action = catchError action (return .show)

-- extractValue :: ThrowsError a -> a
-- extractValue (Right value) = value