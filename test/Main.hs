{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CEKMachine
import Parser
import Syntax
import Test.Hspec
import Text.Trifecta

-- ヘルパー関数
parserTest :: Parser a -> String -> Either String a
parserTest p input = case parseString p mempty input of
  Success a -> Right a
  Failure _ -> Left "Parse failed"

parserSpec :: Spec
parserSpec = do
  describe "Parser" $ do
    describe "parseAtomicExp" $ do
      it "parses integers" $ do
        parserTest parseAtomicExp "42" `shouldBe` Right (Int 42)
        parserTest parseAtomicExp "-42" `shouldBe` Right (Int (-42))

      it "parses booleans" $ do
        parserTest parseAtomicExp "true" `shouldBe` Right (Bool True)
        parserTest parseAtomicExp "false" `shouldBe` Right (Bool False)

      it "parses variables" $ do
        parserTest parseAtomicExp "x" `shouldBe` Right (Var "x")
        parserTest parseAtomicExp "abc" `shouldBe` Right (Var "abc")

      it "rejects reserved words as variables" $ do
        parserTest parseAtomicExp "if" `shouldBe` Left "Parse failed"
        parserTest parseAtomicExp "let" `shouldBe` Left "Parse failed"

    describe "parseFun" $ do
      it "parses simple function" $ do
        parserTest parseFun "fun x -> x"
          `shouldBe` Right (Fun ("x", Nothing) (Var "x"))

      it "parses function with type annotation" $ do
        parserTest parseFun "fun (x : Int) -> x"
          `shouldBe` Right (Fun ("x", Just TyInt) (Var "x"))

      it "parses function with multiple arguments" $ do
        parserTest parseFun "fun x y -> x"
          `shouldBe` Right (Fun ("x", Nothing) (Fun ("y", Nothing) (Var "x")))

      it "parses function with multiple typed arguments" $ do
        parserTest parseFun "fun (x : Int) (y : Bool) -> x"
          `shouldBe` Right (Fun ("x", Just TyInt) (Fun ("y", Just TyBool) (Var "x")))

      it "parses function with mixed typed and untyped arguments" $ do
        parserTest parseFun "fun (x : Int) y (z : Bool) -> x"
          `shouldBe` Right (Fun ("x", Just TyInt) (Fun ("y", Nothing) (Fun ("z", Just TyBool) (Var "x"))))

    describe "parseApp" $ do
      it "parses function application" $ do
        parserTest parseApp "f x"
          `shouldBe` Right (App (Var "f") (Var "x"))

      it "parses multiple application" $ do
        parserTest parseApp "f x y"
          `shouldBe` Right (App (App (Var "f") (Var "x")) (Var "y"))

      it "parses multiple application with complex arguments" $ do
        parserTest parseApp "f (g x) y"
          `shouldBe` Right (App (App (Var "f") (App (Var "g") (Var "x"))) (Var "y"))

    describe "parseLet" $ do
      it "parses simple let binding" $ do
        parserTest parseLet "let x = 1 in x"
          `shouldBe` Right (Let "x" Nothing (Int 1) (Var "x"))

      it "parses let with type annotation" $ do
        parserTest parseLet "let x : Int = 1 in x"
          `shouldBe` Right (Let "x" (Just TyInt) (Int 1) (Var "x"))

      it "parses let with single parameter" $ do
        parserTest parseLet "let f x = x in f"
          `shouldBe` Right (Let "f" Nothing (Fun ("x", Nothing) (Var "x")) (Var "f"))

      it "parses let with multiple parameters" $ do
        parserTest parseLet "let f x y = x in f"
          `shouldBe` Right
            ( Let
                "f"
                Nothing
                (Fun ("x", Nothing) (Fun ("y", Nothing) (Var "x")))
                (Var "f")
            )

      it "parses let with typed parameters" $ do
        parserTest parseLet "let f (x : Int) (y : Bool) = x in f"
          `shouldBe` Right
            ( Let
                "f"
                Nothing
                (Fun ("x", Just TyInt) (Fun ("y", Just TyBool) (Var "x")))
                (Var "f")
            )

      it "parses let with mixed typed and untyped parameters" $ do
        parserTest parseLet "let f (x : Int) y = x in f"
          `shouldBe` Right
            ( Let
                "f"
                Nothing
                (Fun ("x", Just TyInt) (Fun ("y", Nothing) (Var "x")))
                (Var "f")
            )

      it "parses let with type annotation and parameters" $ do
        parserTest parseLet "let f x y : Int -> Int -> Int = y in f 1 2"
          `shouldBe` Right
            ( Let
                "f"
                (Just (TyArr TyInt (TyArr TyInt TyInt)))
                ( Fun
                    ("x", Nothing)
                    ( Fun
                        ("y", Nothing)
                        (Var "y")
                    )
                )
                (App (App (Var "f") (Int 1)) (Int 2))
            )

    describe "Complex expressions with parameterized let" $ do
      it "evaluates let with parameters" $ do
        let e = "let f x y = x in f 1 2"
        parserTest parseExp e
          `shouldBe` Right
            ( Let
                "f"
                Nothing
                (Fun ("x", Nothing) (Fun ("y", Nothing) (Var "x")))
                (App (App (Var "f") (Int 1)) (Int 2))
            )

      it "handles nested parameterized let expressions" $ do
        let e = "let f x = let g y = x in g 2 in f 1"
        parserTest parseExp e
          `shouldBe` Right
            ( Let
                "f"
                Nothing
                ( Fun
                    ("x", Nothing)
                    ( Let
                        "g"
                        Nothing
                        (Fun ("y", Nothing) (Var "x"))
                        (App (Var "g") (Int 2))
                    )
                )
                (App (Var "f") (Int 1))
            )

      it "handles parameterized let with if expressions" $ do
        let e = "let f x y = if x then y else 0 in f true 1"
        parserTest parseExp e
          `shouldBe` Right
            ( Let
                "f"
                Nothing
                ( Fun
                    ("x", Nothing)
                    ( Fun
                        ("y", Nothing)
                        (If (Var "x") (Var "y") (Int 0))
                    )
                )
                (App (App (Var "f") (Bool True)) (Int 1))
            )

    describe "parseIf" $ do
      it "parses if expression" $ do
        parserTest parseIf "if true then 1 else 0"
          `shouldBe` Right (If (Bool True) (Int 1) (Int 0))

    describe "parseTy" $ do
      it "parses basic types" $ do
        parserTest parseTy "Int" `shouldBe` Right TyInt
        parserTest parseTy "Bool" `shouldBe` Right TyBool

      it "parses function types" $ do
        parserTest parseTy "Int -> Bool"
          `shouldBe` Right (TyArr TyInt TyBool)
        parserTest parseTy "Int -> Bool -> Int"
          `shouldBe` Right (TyArr TyInt (TyArr TyBool TyInt))

    describe "Complex expressions" $ do
      it "parses nested let expressions" $ do
        parserTest parseExp "let x = 1 in let y = 2 in x"
          `shouldBe` Right
            ( Let
                "x"
                Nothing
                (Int 1)
                (Let "y" Nothing (Int 2) (Var "x"))
            )

      it "parses function application with if" $ do
        parserTest parseExp "f (if x then 1 else 2)"
          `shouldBe` Right
            ( App
                (Var "f")
                (If (Var "x") (Int 1) (Int 2))
            )

      it "parses multi-argument function with complex body" $ do
        parserTest parseExp "fun x y -> if x then y else 0"
          `shouldBe` Right
            ( Fun
                ("x", Nothing)
                ( Fun
                    ("y", Nothing)
                    (If (Var "x") (Var "y") (Int 0))
                )
            )

      it "parses application of multi-argument function" $ do
        parserTest parseExp "let f = fun x y -> x in f 1 true"
          `shouldBe` Right
            ( Let
                "f"
                Nothing
                (Fun ("x", Nothing) (Fun ("y", Nothing) (Var "x")))
                (App (App (Var "f") (Int 1)) (Bool True))
            )

cekSpec :: Spec
cekSpec = do
  describe "CEK Machine" $ do
    let emptyEnv = [] -- 空のグローバル環境
    describe "Basic evaluation" $ do
      it "evaluates integers" $ do
        eval emptyEnv (Int 42) `shouldBe` Just (VInt 42)
        eval emptyEnv (Int (-10)) `shouldBe` Just (VInt (-10))

      it "evaluates booleans" $ do
        eval emptyEnv (Bool True) `shouldBe` Just (VBool True)
        eval emptyEnv (Bool False) `shouldBe` Just (VBool False)

      it "evaluates variables" $ do
        -- 未束縛の変数はNothingを返す
        eval emptyEnv (Var "x") `shouldBe` Nothing

    describe "Global environment" $ do
      let globalEnv =
            [ ("x", VInt 42),
              ("y", VBool True),
              ("id", VClos (Fun ("z", Nothing) (Var "z")) [])
            ]

      it "finds variables in global environment" $ do
        eval globalEnv (Var "x") `shouldBe` Just (VInt 42)
        eval globalEnv (Var "y") `shouldBe` Just (VBool True)

      it "prefers local bindings over global ones" $ do
        let e = Let "x" Nothing (Int 10) (Var "x")
        eval globalEnv e `shouldBe` Just (VInt 10)

      it "uses global functions" $ do
        let e = App (Var "id") (Int 10)
        eval globalEnv e `shouldBe` Just (VInt 10)

      it "combines local and global bindings" $ do
        let e = Let "z" Nothing (Int 100) (App (Var "id") (Var "x"))
        eval globalEnv e `shouldBe` Just (VInt 42)

    describe "Function evaluation" $ do
      it "evaluates identity function" $ do
        let identityFun = Fun ("x", Nothing) (Var "x")
        let application = App identityFun (Int 42)
        eval emptyEnv application `shouldBe` Just (VInt 42)

      it "evaluates constant function" $ do
        let constFun = Fun ("x", Nothing) (Fun ("y", Nothing) (Var "x"))
        let app1 = App (App constFun (Int 42)) (Int 0)
        eval emptyEnv app1 `shouldBe` Just (VInt 42)

      it "fails when applying non-function" $ do
        let badApp = App (Int 42) (Int 0)
        eval emptyEnv badApp `shouldBe` Nothing

    describe "Let expressions" $ do
      it "evaluates simple let binding" $ do
        let letExp = Let "x" Nothing (Int 42) (Var "x")
        eval emptyEnv letExp `shouldBe` Just (VInt 42)

      it "evaluates nested let bindings" $ do
        let nestedLet =
              Let "x" Nothing (Int 42) $
                Let "y" Nothing (Int 10) $
                  Var "x"
        eval emptyEnv nestedLet `shouldBe` Just (VInt 42)

      it "evaluates let with function definition" $ do
        let letFun =
              Let "f" Nothing (Fun ("x", Nothing) (Var "x")) $
                App (Var "f") (Int 42)
        eval emptyEnv letFun `shouldBe` Just (VInt 42)

    describe "If expressions" $ do
      it "evaluates if with true condition" $ do
        let ifExp = If (Bool True) (Int 42) (Int 0)
        eval emptyEnv ifExp `shouldBe` Just (VInt 42)

      it "evaluates if with false condition" $ do
        let ifExp = If (Bool False) (Int 42) (Int 0)
        eval emptyEnv ifExp `shouldBe` Just (VInt 0)

      it "fails when condition is not boolean" $ do
        let ifExp = If (Int 1) (Int 42) (Int 0)
        eval emptyEnv ifExp `shouldBe` Nothing

    describe "Complex expressions" $ do
      it "evaluates nested function applications" $ do
        let nestedApp =
              Let "f" Nothing (Fun ("x", Nothing) (Var "x")) $
                Let "g" Nothing (Fun ("x", Nothing) (Var "x")) $
                  App (Var "f") (App (Var "g") (Int 42))
        eval emptyEnv nestedApp `shouldBe` Just (VInt 42)

      it "evaluates multi-argument function application" $ do
        let multiArgApp =
              Let
                "f"
                Nothing
                (Fun ("x", Nothing) (Fun ("y", Nothing) (Var "x")))
                $ App (App (Var "f") (Int 42)) (Int 10)
        eval emptyEnv multiArgApp `shouldBe` Just (VInt 42)

      it "evaluates complex conditional expressions" $ do
        let e =
              Let "x" Nothing (Int 10) $
                If
                  ( Let "y" Nothing (Int 20) $
                      Bool True
                  )
                  (Var "x")
                  (Int 0)
        eval emptyEnv e `shouldBe` Just (VInt 10)

      it "evaluates expressions with mixed global and local bindings" $ do
        let globalEnv = [("g", VInt 100)]
        let e =
              Let "f" Nothing (Fun ("x", Nothing) (Var "g")) $
                App (Var "f") (Int 42)
        eval globalEnv e `shouldBe` Just (VInt 100)

    describe "Error cases" $ do
      it "fails on unbound variables" $ do
        let e = App (Var "undefined") (Int 42)
        eval emptyEnv e `shouldBe` Nothing

      it "fails on type errors in if condition" $ do
        let e = If (Int 42) (Int 1) (Int 0)
        eval emptyEnv e `shouldBe` Nothing

      it "fails on type errors in application" $ do
        let e = App (Int 42) (Int 0)
        eval emptyEnv e `shouldBe` Nothing

      it "fails on partial application" $ do
        let e =
              Let
                "f"
                Nothing
                (Fun ("x", Nothing) (Fun ("y", Nothing) (Var "x")))
                (App (Var "f") (Int 42))
        -- 部分適用は値として評価される（クロージャとして）
        case eval emptyEnv e of
          Just (VClos {}) -> pure ()
          _ -> expectationFailure "Should evaluate to closure"

main :: IO ()
main = hspec $ do
  parserSpec
  cekSpec
