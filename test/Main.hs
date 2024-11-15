{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Parser
import TypeChecker
import Syntax
import Data.Either (isLeft)

main :: IO ()
main = hspec $ do
  describe "Type Checker" $ do
    it "should type check simple let binding" $ do
      let input = "let x = 42"
      case parseStmtFromBS input of
        Left err -> error $ "Parse error: " ++ err
        Right stmt -> case typecheck stmt of
          Left err -> error $ "Type error: " ++ show err
          Right (ty, _) -> ty `shouldBe` TyInt

    it "should type check function definition" $ do
      let input = "let f (x: Int) : Int = x"
      case parseStmtFromBS input of
        Left err -> error $ "Parse error: " ++ err
        Right stmt -> case typecheck stmt of
          Left err -> error $ "Type error: " ++ show err
          Right (ty, _) -> ty `shouldBe` TyArr TyInt TyInt

    it "should type check recursive function" $ do
      let input = "let rec fact (n: Int) : Int = if eq n 0 then 1 else mul n (fact (sub n 1))"
      case parseStmtFromBS input of
        Left err -> error $ "Parse error: " ++ err
        Right stmt -> case typecheck stmt of
          Left err -> error $ "Type error: " ++ show err
          Right (ty, _) -> ty `shouldBe` TyArr TyInt TyInt

    it "should reject ill-typed expressions" $ do
      let input = "let x = add true 1"
      case parseStmtFromBS input of
        Left err -> error $ "Parse error: " ++ err
        Right stmt -> typecheck stmt `shouldSatisfy` isLeft
