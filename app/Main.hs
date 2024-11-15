{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import CAMachine
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.IORef
import Data.List
import Parser
import Syntax
import System.Console.Haskeline
import TypeChecker

-- REPLの状態
data ReplState = ReplState
  { typeEnv :: TypeEnv, -- 型環境
    valueEnv :: GlobalEnv -- 値環境
  }

-- REPLモナド
type Repl a = (ReaderT (IORef ReplState) (InputT IO)) a

-- 初期状態の作成
initialReplState :: ReplState
initialReplState =
  ReplState
    { typeEnv = initialTypeEnv,
      valueEnv = primitives
    }

-- REPLの設定
replSettings :: Settings IO
replSettings =
  Settings
    { complete =
        completeWord
          Nothing
          " \t"
          (\str -> return $ map simpleCompletion $ filter (str `isPrefixOf`) commands),
      historyFile = Just ".efl_history",
      autoAddHistory = True
    }
  where
    commands = [":type", ":quit"]

-- REPLの実行
runRepl :: IO ()
runRepl = do
  ref <- newIORef initialReplState
  runInputT replSettings (runReaderT repl ref)

-- REPLのメインループ
repl :: Repl ()
repl = do
  minput <- lift $ getInputLine "efl> "
  case minput of
    Nothing -> return ()
    Just input
      | ":quit" `isPrefixOf` input -> return ()
      | ":type " `isPrefixOf` input -> do
          handleTypeQuery (drop 6 input)
          repl
      | otherwise -> do
          handleInput (BS.pack input)
          repl

-- 型クエリの処理
handleTypeQuery :: String -> Repl ()
handleTypeQuery input = do
  case parseExpFromBS (BS.pack input) of
    Left err -> liftIO $ putStrLn $ "Parse error: " ++ err
    Right expr -> do
      ReplState {..} <- liftIO . readIORef =<< ask
      case typeCheck typeEnv expr of
        Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
        Right ty -> liftIO $ putStrLn $ "Type: " ++ show ty

-- 入力の処理
handleInput :: ByteString -> Repl ()
handleInput input = do
  case parseStmtFromBS input of
    Left err -> do
      -- 文としてのパースが失敗した場合、式としてパースを試みる
      case parseExpFromBS input of
        Left _ -> liftIO $ putStrLn $ "Parse error: " ++ err
        Right expr -> handleExp expr
    Right stmt -> handleStmt stmt

-- 式の処理
handleExp :: Exp -> Repl ()
handleExp expr = do
  ReplState {..} <- liftIO . readIORef =<< ask
  case typeCheck typeEnv expr of
    Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
    Right ty -> do
      case eval valueEnv expr of
        Left err -> liftIO $ putStrLn $ "Runtime error: " ++ show err
        Right value -> liftIO $ do
          putStrLn $ "Value: " ++ show value
          putStrLn $ "Type: " ++ show ty

-- 文の処理
handleStmt :: Stmt -> Repl ()
handleStmt stmt = do
  ref <- ask
  ReplState {..} <- liftIO $ readIORef ref
  case typeCheckStmt typeEnv stmt of
    Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
    Right (newTypeEnv, ty) -> do
      case evalStmt valueEnv stmt of
        Left err -> liftIO $ putStrLn $ "Runtime error: " ++ show err
        Right (newValueEnv, value) -> do
          -- 状態の更新
          liftIO . writeIORef ref $
            ReplState
              { typeEnv = newTypeEnv,
                valueEnv = newValueEnv
              }
          -- 結果の表示
          liftIO $ do
            putStrLn $ "Value: " ++ show value
            putStrLn $ "Type: " ++ show ty

-- 文の評価
evalStmt :: GlobalEnv -> Stmt -> Either CAMError (GlobalEnv, Value)
evalStmt env = \case
  LetStmt mname _ e -> do
    value <- eval env e
    case mname of
      Just name -> return ((name, value) : env, value)
      Nothing -> return (env, value)
  LetRecStmt name (param, paramTy) ty body -> do
    let e = LetRec name (param, paramTy) ty body (Var name)
    value <- eval env e
    return ((name, value) : env, value)

main :: IO ()
main = runRepl
