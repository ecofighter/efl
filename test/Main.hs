{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import TypeChecker
import Syntax
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "TypeChecker" $ do
    describe "checkExp" $ do
      it "checks simple literals correctly" $ do
        checkExp initialEnv (Int 42) `shouldBe` Right TyInt
        checkExp initialEnv (Bool True) `shouldBe` Right TyBool
        checkExp initialEnv Unit `shouldBe` Right TyUnit

      it "checks arithmetic operations correctly" $ do
        let addApp = App (App (Var "add") (Int 1)) (Int 2)
        checkExp initialEnv addApp `shouldBe` Right TyInt

        let subApp = App (App (Var "sub") (Int 3)) (Int 4)
        checkExp initialEnv subApp `shouldBe` Right TyInt

      it "checks comparisons correctly" $ do
        let eqApp = App (App (Var "eq") (Int 1)) (Int 2)
        checkExp initialEnv eqApp `shouldBe` Right TyBool

        let ltApp = App (App (Var "lt") (Int 3)) (Int 4)
        checkExp initialEnv ltApp `shouldBe` Right TyBool

      it "detects type errors in arithmetic operations" $ do
        let invalidAdd = App (App (Var "add") (Bool True)) (Int 2)
        case checkExp initialEnv invalidAdd of
          Left (UnificationError TyInt TyBool) -> return ()
          other -> expectationFailure $ "Expected type error, but got: " ++ show other

      it "checks if expressions correctly" $ do
        let ifExp = If (Bool True) (Int 1) (Int 2)
        checkExp initialEnv ifExp `shouldBe` Right TyInt

        let invalidIf = If (Int 1) (Bool True) (Bool False)
        case checkExp initialEnv invalidIf of
          Left (UnificationError TyInt TyBool) -> return ()
          other -> expectationFailure $ "Expected type error, but got: " ++ show other

      it "checks function application correctly" $ do
        let funExp = Fun ("x", TyInt) (Var "x")
        let appExp = App funExp (Int 42)
        checkExp initialEnv appExp `shouldBe` Right TyInt

      it "checks pairs correctly" $ do
        let pairExp = Pair (Int 1) (Bool True)
        checkExp initialEnv pairExp `shouldBe` Right (TyProd TyInt TyBool)

        let fstExp = Fst (Pair (Int 1) (Bool True))
        checkExp initialEnv fstExp `shouldBe` Right TyInt

        let sndExp = Snd (Pair (Int 1) (Bool True))
        checkExp initialEnv sndExp `shouldBe` Right TyBool

      it "checks polymorphic identity function" $ do
        -- let id = fun x -> x
        let idFun = Let (Just "id") (TyVar 0) 
                       (Fun ("x", TyVar 1) (Var "x"))
                       (Var "id")
        case checkExp initialEnv idFun of
          Right (TyArr a b) -> do
            a `shouldBe` b  -- 入力と出力の型が同じであることを確認
          Right ty -> expectationFailure $ "Expected function type, but got: " ++ show ty
          Left err -> expectationFailure $ "Failed to check id function: " ++ show err

        -- id applied to different types
        let env = case checkStmt initialEnv (LetStmt (Just "id") (TyVar 0) (Fun ("x", TyVar 1) (Var "x"))) of
              Right (env', _) -> env'
              Left err -> error $ "Failed to create env: " ++ show err
        
        checkExp env (App (Var "id") (Int 42)) `shouldBe` Right TyInt
        checkExp env (App (Var "id") (Bool True)) `shouldBe` Right TyBool
        checkExp env (App (Var "id") Unit) `shouldBe` Right TyUnit

      it "checks polymorphic pair operations" $ do
        -- let swap = fun p -> (snd p, fst p)
        let swapFun = Let (Just "swap") (TyVar 10000)
                         (Fun ("p", TyProd (TyVar 10001) (TyVar 10002))
                              (Pair (Snd (Var "p")) (Fst (Var "p"))))
                         (Var "swap")
        case checkExp initialEnv swapFun of
          Right ty -> case ty of
            TyArr (TyProd a b) (TyProd c d) -> do
              b `shouldBe` c
              a `shouldBe` d
            _ -> expectationFailure $ "Expected function type, but got: " ++ show ty
          Left err -> expectationFailure $ "Failed to check swap function: " ++ show err

    describe "checkStmt" $ do
      it "checks let statements correctly" $ do
        let letStmt = LetStmt (Just "x") TyInt (Int 42)
        case checkStmt initialEnv letStmt of
          Right (env', ty) -> do
            ty `shouldBe` TyInt
            Map.member "x" env' `shouldBe` True
          Left err -> expectationFailure $ "Expected successful type checking, but got error: " ++ show err

      it "checks polymorphic let statements" $ do
        let idLetStmt = LetStmt (Just "id") (TyVar 0) (Fun ("x", TyVar 1) (Var "x"))
        case checkStmt initialEnv idLetStmt of
          Right (env', ty) -> do
            Map.member "id" env' `shouldBe` True
            case ty of
              TyArr a b -> a `shouldBe` b
              _ -> expectationFailure $ "Expected function type, but got: " ++ show ty
          Left err -> expectationFailure $ "Expected successful type checking, but got error: " ++ show err

      it "checks let rec statements correctly" $ do
        let factorial = 
              LetRecStmt "fact" ("n", TyInt) TyInt
                (If (App (App (Var "eq") (Var "n")) (Int 0))
                    (Int 1)
                    (App (App (Var "mul") (Var "n"))
                         (App (Var "fact") (App (App (Var "sub") (Var "n")) (Int 1)))))
        case checkStmt initialEnv factorial of
          Right (env', ty) -> do
            ty `shouldBe` TyArr TyInt TyInt
            Map.member "fact" env' `shouldBe` True
          Left err -> expectationFailure $ "Expected successful type checking, but got error: " ++ show err

      it "checks polymorphic recursive functions" $ do
        -- let rec length l = if is_empty l then 0 else 1 + length (tail l)
        let lengthFun = 
              LetRecStmt "length" ("l", TyVar 0) TyInt
                (If (Var "is_empty")  -- 実際のis_emptyは実装されていないので、型エラーになるはず
                    (Int 0)
                    (App (App (Var "add") (Int 1))
                         (App (Var "length") (App (Var "tail") (Var "l")))))
        case checkStmt initialEnv lengthFun of
          Right _ -> expectationFailure "Expected type error due to missing is_empty function"
          Left _ -> return ()  -- エラーが発生することを期待

        -- let rec map f xs = if is_empty xs then [] else cons (f (head xs)) (map f (tail xs))
        let mapFun =
              LetRecStmt "map" ("f", TyArr (TyVar 0) (TyVar 1)) (TyArr (TyVar 2) (TyVar 3))
                (Fun ("xs", TyVar 2)
                     (If (Var "is_empty")  -- 同様にis_emptyは未実装
                         Unit  -- リストが未実装なのでUnitを使用
                         (App (App (Var "cons")
                                  (App (Var "f") (App (Var "head") (Var "xs"))))
                              (App (App (Var "map") (Var "f"))
                                   (App (Var "tail") (Var "xs"))))))
        case checkStmt initialEnv mapFun of
          Right _ -> expectationFailure "Expected type error due to missing list operations"
          Left _ -> return ()  -- エラーが発生することを期待

    describe "generalize and instantiate" $ do
      it "generalizes and instantiates types correctly" $ do
        let env = initialEnv
            e = Fun ("x", TyInt) (Var "x")
        case checkExp env e of
          Right ty -> do
            case runTypeCheck (generalize env ty >>= instantiate) of
              Right ty' -> ty' `shouldBe` ty
              Left err -> expectationFailure $ "Failed to generalize/instantiate: " ++ show err
          Left err -> expectationFailure $ "Failed to check expression: " ++ show err

      it "generalizes polymorphic functions correctly" $ do
        let env = initialEnv
            e = Fun ("x", TyVar 0) (Var "x")  -- 多相的な恒等関数
        case checkExp env e of
          Right ty1 -> do
            case runTypeCheck (generalize env ty1 >>= instantiate) of
              Right ty2 -> do
                case (ty1, ty2) of
                  (TyArr a b, TyArr c d) -> do
                    a `shouldBe` b  -- 入力と出力の型が同じ
                    c `shouldBe` d  -- インスタンス化後も同じ型
                  _ -> expectationFailure "Expected arrow type"
              Left err -> expectationFailure $ "Failed to generalize/instantiate: " ++ show err
          Left err -> expectationFailure $ "Failed to check expression: " ++ show err

main :: IO ()
main = hspec spec
