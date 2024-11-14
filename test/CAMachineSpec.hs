{-# LANGUAGE OverloadedStrings #-}
module CAMachineSpec(caMachineSpec) where

import Test.Hspec
import CAMachine
import Syntax

caMachineSpec :: Spec
caMachineSpec = do
  describe "CAMachine evaluation" $ do
    context "basic values" $ do
      it "evaluates integer literals" $ do
        evalWithPrimitives (Int 42) `shouldBe` Right (VInt 42)
        evalWithPrimitives (Int (-10)) `shouldBe` Right (VInt (-10))

      it "evaluates boolean literals" $ do
        evalWithPrimitives (Bool True) `shouldBe` Right (VBool True)
        evalWithPrimitives (Bool False) `shouldBe` Right (VBool False)

    context "arithmetic operations" $ do
      it "performs addition" $ do
        let expr = App (App (Var "add") (Int 3)) (Int 4)
        evalWithPrimitives expr `shouldBe` Right (VInt 7)

      it "performs subtraction" $ do
        let expr = App (App (Var "sub") (Int 10)) (Int 4)
        evalWithPrimitives expr `shouldBe` Right (VInt 6)

      it "performs multiplication" $ do
        let expr = App (App (Var "mul") (Int 3)) (Int 4)
        evalWithPrimitives expr `shouldBe` Right (VInt 12)

      it "performs division" $ do
        let expr = App (App (Var "div") (Int 10)) (Int 2)
        evalWithPrimitives expr `shouldBe` Right (VInt 5)

      it "handles division by zero" $ do
        let expr = App (App (Var "div") (Int 10)) (Int 0)
        case evalWithPrimitives expr of
          Left (RuntimeError _) -> pure ()
          other -> expectationFailure $ "Expected division by zero error, got: " ++ show other

    context "comparison operations" $ do
      it "performs equality comparison" $ do
        let expr = App (App (Var "eq") (Int 3)) (Int 3)
        evalWithPrimitives expr `shouldBe` Right (VBool True)

      it "performs less than comparison" $ do
        let expr = App (App (Var "lt") (Int 3)) (Int 4)
        evalWithPrimitives expr `shouldBe` Right (VBool True)

      it "performs greater than comparison" $ do
        let expr = App (App (Var "gt") (Int 5)) (Int 3)
        evalWithPrimitives expr `shouldBe` Right (VBool True)

    context "functions" $ do
      it "evaluates simple function application" $ do
        let expr = App (Fun ("x", Nothing) (Var "x")) (Int 42)
        evalWithPrimitives expr `shouldBe` Right (VInt 42)

      it "handles nested function applications" $ do
        let expr = App 
                    (Fun ("x", Nothing) 
                      (Fun ("y", Nothing) 
                        (App (App (Var "add") (Var "x")) (Var "y"))))
                    (Int 3)
        let applied = App expr (Int 4)
        evalWithPrimitives applied `shouldBe` Right (VInt 7)

    context "let expressions" $ do
      it "evaluates simple let bindings" $ do
        let expr = Let (Just "x") Nothing (Int 42) (Var "x")
        evalWithPrimitives expr `shouldBe` Right (VInt 42)

      it "handles nested let bindings" $ do
        let expr = Let (Just "x") Nothing (Int 10)
                    (Let (Just "y") Nothing (Int 20)
                      (App (App (Var "add") (Var "x")) (Var "y")))
        evalWithPrimitives expr `shouldBe` Right (VInt 30)

    context "conditional expressions" $ do
      it "evaluates if-then-else with true condition" $ do
        let expr = If (Bool True) (Int 1) (Int 2)
        evalWithPrimitives expr `shouldBe` Right (VInt 1)

      it "evaluates if-then-else with false condition" $ do
        let expr = If (Bool False) (Int 1) (Int 2)
        evalWithPrimitives expr `shouldBe` Right (VInt 2)

      it "evaluates complex conditions" $ do
        let expr = If (App (App (Var "lt") (Int 3)) (Int 4))
                     (Int 1)
                     (Int 2)
        evalWithPrimitives expr `shouldBe` Right (VInt 1)

    context "pairs" $ do
      it "creates and accesses pairs" $ do
        let expr = Pair (Int 1) (Int 2)
        evalWithPrimitives expr `shouldBe` Right (VPair (VInt 1) (VInt 2))

      it "extracts first element of pair" $ do
        let expr = Fst (Pair (Int 1) (Int 2))
        evalWithPrimitives expr `shouldBe` Right (VInt 1)

      it "extracts second element of pair" $ do
        let expr = Snd (Pair (Int 1) (Int 2))
        evalWithPrimitives expr `shouldBe` Right (VInt 2)

      it "handles nested pairs" $ do
        let expr = Pair (Pair (Int 1) (Int 2)) (Int 3)
        evalWithPrimitives expr `shouldBe` 
          Right (VPair (VPair (VInt 1) (VInt 2)) (VInt 3))

    context "error cases" $ do
      it "handles unbound variables" $ do
        let expr = Var "nonexistent"
        case evalWithPrimitives expr of
          Left err -> err `shouldBe` UnboundVariable "nonexistent"
          Right v -> expectationFailure $ "Expected error but got: " ++ show v

      it "handles type errors in arithmetic" $ do
        let expr = App (App (Var "add") (Bool True)) (Int 1)
        case evalWithPrimitives expr of
          Left (RuntimeError _) -> pure ()
          other -> expectationFailure $ "Expected type error, got: " ++ show other

      it "handles type errors in conditionals" $ do
        let expr = If (Int 1) (Int 2) (Int 3)
        case evalWithPrimitives expr of
          Left (TypeError "Test: Expected boolean") -> pure ()
          other -> expectationFailure $ "Expected type error, got: " ++ show other
