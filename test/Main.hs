{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CEKMachine
import Syntax
import Test.Hspec

import TypeCheckerSpec (typeCheckerSpec)
import ParserSpec (parserSpec)


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
  typeCheckerSpec
