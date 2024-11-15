{-# LANGUAGE OverloadedStrings #-}

module Main where

import CAMachine
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map qualified as Map
import Parser
import Syntax
import System.Console.Haskeline
import TypeChecker

data REPLState = REPLState
  { typeEnv :: TypeEnv,
    globalEnv :: GlobalEnv
  }

initialState :: REPLState
initialState =
  REPLState
    { typeEnv = primitivesTypes,
      globalEnv = primitives
    }

process :: String -> REPLState -> IO (Either String (REPLState, ByteString, Value))
process input state = do
  case parseStmtFromBS (BS.pack input) of
    Left err -> return $ Left $ "Parse error: " ++ err
    Right stmt -> case typecheck' (typeEnv state) stmt of
      Left err -> return $ Left $ "Type error: " ++ show err
      Right (inferredTy, _) -> case stmt of
        LetStmt name _ expr -> do
          case eval (globalEnv state) expr of
            Left err -> return $ Left $ "Evaluation error: " ++ show err
            Right val -> do
              let newTypeEnv = case name of
                    Just n -> Map.insert n (generalize (typeEnv state) inferredTy) (typeEnv state)
                    Nothing -> typeEnv state
                  newGlobalEnv = case name of
                    Just n -> (n, val) : globalEnv state
                    Nothing -> globalEnv state
              return $ Right (state {typeEnv = newTypeEnv, globalEnv = newGlobalEnv}, BS.pack (show inferredTy), val)
        LetRecStmt name (param, paramTy) retTy body -> do
          let funTy = TyArr paramTy retTy
              expr = LetRec name (param, paramTy) retTy body (Var name)
          case eval (globalEnv state) expr of
            Left err -> return $ Left $ "Evaluation error: " ++ show err
            Right val -> do
              let newTypeEnv = Map.insert name (generalize (typeEnv state) funTy) (typeEnv state)
                  newGlobalEnv = (name, val) : globalEnv state
              return $ Right (state {typeEnv = newTypeEnv, globalEnv = newGlobalEnv}, BS.pack (show funTy), val)

repl :: IO ()
repl = runInputT defaultSettings (loop initialState)
  where
    loop :: REPLState -> InputT IO ()
    loop state = do
      minput <- getInputLine "# "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          result <- liftIO $ process input state
          case result of
            Left err -> do
              outputStrLn $ "Error: " ++ err
              loop state
            Right (newState, ty, val) -> do
              outputStrLn $ "Type: " ++ BS.unpack ty
              outputStrLn $ "Value: " ++ show val
              loop newState

main :: IO ()
main = do
  putStrLn "Welcome to the MinCaml REPL!"
  putStrLn "Enter expressions or :q to quit."
  repl
