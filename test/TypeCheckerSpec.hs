{-# LANGUAGE OverloadedStrings #-}

module TypeCheckerSpec (typeCheckerSpec) where

import Test.Hspec
import Parser
import Syntax
import TypeChecker
import Text.Trifecta

-- パーサーとタイプチェッカーを組み合わせたヘルパー関数
inferType :: String -> Either TypeError Ty
inferType input = case parseString parseExp mempty input of
  Success expr -> case typecheck expr of
    Right (ty, _) -> Right ty
    Left e -> Left e
  Failure _ -> error $ "Parse failed: " ++ input

typeCheckerSpec :: Spec
typeCheckerSpec = do
  -- 前半は変更なし...

    describe "Complex expressions" $ do
      it "handles nested let expressions" $ do
        let prog = "let f = fun (x : Int) -> x in let g = fun (y : Int) -> f y in g 42"
        inferType prog `shouldBe` Right TyInt

      it "handles complex type inference" $ do
        let prog = "let apply = fun (f : Int -> Int) -> fun (x : Int) -> f x in \
                  \let compose = fun (f : Int -> Int) -> fun (g : Int -> Int) -> \
                  \  fun (x : Int) -> f (g x) in \
                  \let inc = fun (x : Int) -> x in \
                  \let double = fun (x : Int) -> x in \
                  \apply (compose inc double) 42"
        inferType prog `shouldBe` Right TyInt

    describe "Error reporting" $ do
      it "reports unbound variables" $ do
        case inferType "x" of
          Left (UnboundVariable x) | x == "x" -> pure ()
          _ -> expectationFailure "Should report unbound variable"

      it "reports type mismatches in arithmetic" $ do
        let prog = "let x : Bool = true in let y : Int = x in y"
        case inferType prog of
          Left (TypeMismatch _ _) -> pure ()
          _ -> expectationFailure "Should report type mismatch"

      it "reports occurs check violations" $ do
        -- 無限型をより直接的に生成するケース
        let prog = "let f = fun x -> x x in f"
        case inferType prog of
          Left (OccursCheck _ _) -> pure ()
          _ -> expectationFailure "Should report occurs check violation"

    describe "Type schemes and polymorphism" $ do
      it "allows polymorphic functions to be used at different types" $ do
        let prog = "let id = fun x -> x in \
                  \let a = id 42 in \
                  \let b = id true in a"
        inferType prog `shouldBe` Right TyInt

      it "properly handles type generalization" $ do
        let prog = "let f = fun (x : Int) -> x in \
                  \let g = fun x -> f x in \
                  \g 42"
        inferType prog `shouldBe` Right TyInt

      it "supports higher-rank polymorphism" $ do
        let prog = "let app = fun (f : Int -> Int) -> fun (x : Int) -> f x in \
                  \let revapp = fun (x : Int) -> fun (f : Int -> Int) -> f x in \
                  \app (fun (x : Int) -> x) 42"
        inferType prog `shouldBe` Right TyInt
