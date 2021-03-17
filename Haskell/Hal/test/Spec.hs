import Test.Hspec
import Control.Exception (evaluate)

import Parser
import ErrorManagement

main :: IO ()
main = hspec $ do
    describe "Test Cons" $ do
        it "Test (cons 1 2)" $ do
            evalFile "(cons 1 2)" `shouldBe` "(1 . 2)"
        it "Test (cons (cons 1 2) (cons 3 4))" $ do
            evalFile "(cons (cons 1 2) (cons 3 4))" `shouldBe` "((1 . 2) . (3 . 4))"
        it "Test (cons 1 (cons 2 3))" $ do
            evalFile "(cons 1 (cons 2 3))" `shouldBe` "(1 2 . 3)"
        it "Test (cons 1 (cons 2 (cons 3 '())))" $ do
            evalFile "(cons 1 (cons 2 (cons 3 '())))" `shouldBe` "(1 2 3)"

    describe "Test List" $ do
        it "Test (list 1)" $ do
            evalFile "(list 1)" `shouldBe` "(1)"
        it "Test (list 1 2 3)" $ do
            evalFile "(list 1 2 3)" `shouldBe` "(1 2 3)"

    describe "Test Quote" $ do
        it "Test '()" $ do
            evalFile "'()" `shouldBe` "()"
        it "Test 'toto" $ do
            evalFile "'toto" `shouldBe` "toto"
        it "Test '(toto)" $ do
            evalFile "'(toto)" `shouldBe` "(toto)"
        it "Test (quote (+ 1 2))" $ do
            evalFile "(quote (+ 1 2))" `shouldBe` "(+ 1 2)"
                            
    describe "Test Car" $ do
        it "Test (car (cons 1 2))" $ do
            evalFile "(car (cons 1 2))" `shouldBe` "1"
        it "Test (car '(1 2 3))" $ do
            evalFile "(car '(1 2 3))" `shouldBe` "1"
    
    describe "Test Cdr" $ do
        it "Test (cdr (cons 1 2))" $ do
            evalFile "(cdr (cons 1 2))" `shouldBe` "2"
        it "Test (cdr '(1 2 3))" $ do
            evalFile "(cdr '(1 2 3))" `shouldBe` "(2 3)"

    describe "Test Equal" $ do
        it "Test (eq? 1 1)" $ do
            evalFile "(eq? 1 1)" `shouldBe` "#t"
        it "Test  (eq? (+ 1 1) 2)" $ do
            evalFile "(eq? (+ 1 1) 2)" `shouldBe` "#t"
        it "Test (eq? 'foo (car '(foo bar)))" $ do
            evalFile "(eq? 'foo (car '(foo bar)))" `shouldBe` "#t"
        it "Test (eq? 'foo 'bar)" $ do
            evalFile "(eq? 'foo 'bar)" `shouldBe` "#f"
        it "Test  (eq? '() '())" $ do
            evalFile " (eq? '() '())" `shouldBe` "#t"

    describe "Test Atom" $ do
        it "Test (atom? 'foo)" $ do
            evalFile "(atom? 'foo)" `shouldBe` "#t"
        it "Test (atom? '(1 2 3))" $ do
            evalFile "(atom? '(1 2 3))" `shouldBe` "#f"
        it "Test (atom? '())" $ do
            evalFile "(atom? '())" `shouldBe` "#t"

    describe "Test Addition" $ do
        it "Test (+ 1 1)" $ do
            evalFile "(+ 1 1)" `shouldBe` "2"
        it "Test (+ -2 1)" $ do
            evalFile "(+ -2 1)" `shouldBe` "-1"
        it "Test (+ -2 -2)" $ do
            evalFile "(+ -2 -2)" `shouldBe` "-4"

    describe "Test Soustraction" $ do
        it "Test (- 1 1)" $ do
            evalFile "(- 1 1)" `shouldBe` "0"
        it "Test (- -2 1)" $ do
            evalFile "(- -2 1)" `shouldBe` "-3"
        it "Test (- -2 -2)" $ do
            evalFile "(- -2 -2)" `shouldBe` "0"

    describe "Test Multiplication" $ do
        it "Test (* 5 2)" $ do
            evalFile "(* 5 2)" `shouldBe` "10"
        it "Test (* -2 3)" $ do
            evalFile "(* -2 3)" `shouldBe` "-6"
        it "Test (* -2 -2)" $ do
            evalFile "(* -2 -2)" `shouldBe` "4"
    
    describe "Test Division" $ do
        it "Test (div 5 2)" $ do
            evalFile "(div 5 2)" `shouldBe` "2"
        it "Test (div -2 3)" $ do
            evalFile "(div -2 3)" `shouldBe` "-1"
        it "Test (div -2 -2)" $ do
            evalFile "(div -2 -2)" `shouldBe` "1"

        -- describe "Test Division" $ do
        --     it "Test (* 5 2)" $ do
        --         evalFile "(* 5 2)" `shouldBe` "10"
        --     it "Test (* -2 3)" $ do
        --         evalFile "(* -2 3)" `shouldBe` "-6"
        --     it "Test (* -2 -2)" $ do
        --         evalFile "(* -2 -2)" `shouldBe` "4"
        