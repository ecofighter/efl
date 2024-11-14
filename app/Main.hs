{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.Console.Haskeline
import Text.Trifecta hiding (err)
import qualified Data.Map as Map

import CEKMachine
import Parser
import TypeChecker
import Syntax

-- REPLの状態
data ReplState = ReplState
  { replTypeEnv :: Map.Map ByteString TyScheme  -- 型環境
  , replValueEnv :: GlobalEnv                    -- 値環境
  }

-- 初期状態
initialReplState :: ReplState
initialReplState = ReplState
  { replTypeEnv = Map.empty
  , replValueEnv = []
  }

-- REPL monad
type Repl a = InputT IO a

-- メイン関数
main :: IO ()
main = runInputT defaultSettings $ do
  outputStrLn "Welcome to MinML REPL!"
  outputStrLn "Type :h for help"
  repl initialReplState

-- REPLのメインループ
repl :: ReplState -> Repl ()
repl state = do
  minput <- getInputLine "minml> "
  case minput of
    Nothing -> return ()
    Just input
      | input == ":q" -> return ()
      | input == ":h" -> do
          showHelp
          repl state
      | otherwise -> do
          case parseCommand input of
            Left err -> do
              outputStrLn $ "Parse error: " ++ show err
              repl state
            Right stmt -> do
              newState <- processStmt state stmt
              repl newState

-- コマンドのパース
parseCommand :: String -> Either String Stmt
parseCommand input = case parseString parseStmt mempty input of
  Success stmt -> Right stmt
  Failure err -> Left (show err)

-- ヘルプの表示
showHelp :: Repl ()
showHelp = do
  outputStrLn "Available commands:"
  outputStrLn "  :h  - show this help"
  outputStrLn "  :q  - quit"
  outputStrLn "Examples:"
  outputStrLn "  let x : Int = 42"
  outputStrLn "  let add : Int -> Int -> Int = fun x y -> x + y"
  outputStrLn "  1 + 2"

-- 文の処理
processStmt :: ReplState -> Stmt -> Repl ReplState
processStmt state@ReplState{..} stmt = case stmt of
  LetStmt name tyAnn expr -> do
    -- 型検査 (型注釈がある場合はそれを使用)
    case typecheck' (replTypeEnv <> primitivesTypes) expr tyAnn of
      Left err -> do
        outputStrLn $ "Type error: " ++ show err
        return state
      Right (ty, _) -> do
        -- 評価
        case eval (replValueEnv <> primitives) expr of
          Nothing -> do
            outputStrLn "Evaluation error"
            return state
          Just val -> do
            -- 型を表示
            outputStrLn $ BS.unpack name ++ " : " ++ show ty
            -- 値を表示
            outputStrLn $ BS.unpack name ++ " = " ++ show val
            -- 環境を更新
            let scheme = case tyAnn of
                  Just annotTy -> Forall [] annotTy  -- 型注釈がある場合はそれを使用
                  Nothing -> generalize replTypeEnv ty  -- ない場合は推論した型を一般化
            let newTypeEnv = Map.insert name scheme replTypeEnv
            let newValueEnv = (name, val) : replValueEnv
            return $ state { replTypeEnv = newTypeEnv, replValueEnv = newValueEnv }

  ExpStmt expr -> do
    -- 型検査
    case typecheck' (replTypeEnv <> primitivesTypes) expr Nothing of
      Left err -> do
        outputStrLn $ "Type error: " ++ show err
        return state
      Right (ty, _) -> do
        -- 評価
        case eval (replValueEnv <> primitives) expr of
          Nothing -> do
            outputStrLn "Evaluation error"
            return state
          Just val -> do
            -- 型と値を表示
            outputStrLn $ "- : " ++ show ty
            outputStrLn $ "= " ++ show val
            return state

-- エラー表示用のヘルパー関数
showError :: Show e => e -> String
showError = show
