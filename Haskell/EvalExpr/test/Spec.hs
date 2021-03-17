import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)
import ErrorSpec

main :: IO ()
main = hspec $ do
    describe "basicErrorManagement" $ do
        it "returns 84 if too much arguments" $ do
            checkArgs ["3+4", "3"] `shouldBe` (84 :: Int)
        it "returns 84 if not enough argument" $ do
            checkArgs [] `shouldBe` (84 :: Int)

    describe "basicSyntaxErrorManagement" $ do
        it "returns False if empty string" $ do
            illegal "" `shouldBe` (False :: Bool)

        it "returns False if parenthesis problem" $ do
            illegal "(1+3" `shouldBe` (False :: Bool)
        it "returns False if too much parenthesis" $ do
            illegal "((1+3)" `shouldBe` (False :: Bool)

        it "returns a string without spaces" $ do
            (concat $ words "1 +3   ") `shouldBe` ("1+3" :: String)
        it "returns a string without tabs" $ do
            (concat $ words "\t1\t\t\t+3") `shouldBe` ("1+3" :: String)

    describe "mediumSyntaxErrorManagement" $ do
        it "returns False if illegal character (alphabet)" $ do
            illegal "1a+3" `shouldBe` (False :: Bool)
        it "returns False if illegal character (modulo)" $ do
            illegal "1%3" `shouldBe` (False :: Bool)
        it "returns False if illegal character (comma)" $ do
            illegal "1,1+3" `shouldBe` (False :: Bool)
        it "returns True when expression is correct : .()+-*/^' '0-9" $ do
            illegal "1.3+3" `shouldBe` (True :: Bool)

    describe "operatorsSyntaxErrorManagement" $ do
        it "returns False if two operands in a row (+)" $ do
            checkOperation "1++3" False `shouldBe` (False :: Bool)
        it "returns False if two operands in a row (-)" $ do
            checkOperation "1--3" False `shouldBe` (False :: Bool)
        it "returns False if two operands in a row (*)" $ do
            checkOperation "1**3" False `shouldBe` (False :: Bool)
        it "returns False if two operands in a row (/)" $ do
            checkOperation "1//3" False `shouldBe` (False :: Bool)
        it "returns False if two operands in a row (^)" $ do
            checkOperation "1^^3" False `shouldBe` (False :: Bool)

    describe "operandsSyntaxErrorManagement" $ do
        it "returns False if space between two operands" $ do
            checkNum "1 0+3" False False `shouldBe` (False :: Bool)
        it "returns False if space between two operands even if there are spaces in the expression" $ do
            checkNum " 1 0 + 3 " False False `shouldBe` (False :: Bool)
        it "reuturn Fals if 2 space or more is chained" $ do
            checkBlank "       -12     " False `shouldBe` (False :: Bool)

    describe "myRound" $ do
        it "returns a double with 11  nb after dot" $ do
            myRound 2 (3.66666666667 :: Double) `shouldBe` (3.67 :: Double)
        it "returns a double with 3 nb after dot" $ do
            myRound 2 (-5.345 :: Double) `shouldBe` (-5.34 :: Double)
        it "returns a double with 1 nb after dot" $ do
            myRound 2 (3.6 :: Double) `shouldBe` (3.60 :: Double)
        it "returns a double with 0 nb after dot" $ do
            myRound 2 (3 :: Double) `shouldBe` (3.00 :: Double)
    