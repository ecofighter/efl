{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import CAMachine
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map qualified as Map
import Parser
import Syntax
import System.Console.Haskeline
import Text.Trifecta (Result (..), parseByteString)
import TypeChecker

type TypeEnv = Map.Map ByteString TyScheme

data REPLState = REPLState
  { typeEnv :: TypeEnv,
    globalEnv :: GlobalEnv
  }

initialREPLState :: REPLState
initialREPLState =
  REPLState
    { typeEnv = primitivesTypes,
      globalEnv = primitives
    }

main :: IO ()
main = runInputT defaultSettings $ loop initialREPLState
  where
    loop :: REPLState -> InputT IO ()
    loop state = do
      minput <- getInputLine "λ> "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          case parseInput (BS.pack input) of
            Success stmt -> do
              result <- liftIO $ evalStmt state stmt
              case result of
                Left err -> outputStrLn $ "Error: " ++ show err
                Right newState -> do
                  loop newState
            Failure err -> do
              outputStrLn $ "Parse error: " ++ show err
              loop state

parseInput :: ByteString -> Result Stmt
parseInput = parseByteString parseStmt mempty

evalStmt :: REPLState -> Stmt -> IO (Either String REPLState)
evalStmt state@REPLState {..} stmt = case stmt of
  ExpStmt expr -> do
    case typecheck' typeEnv expr Nothing of
      Left err -> return $ Left $ "Type error: " ++ show err
      Right (ty, _) -> do
        case eval globalEnv expr of
          Left err -> return $ Left $ "Runtime error: " ++ show err
          Right val -> do
            putStrLn $ "Type: " ++ show ty
            putStrLn $ "Value: " ++ show val
            return $ Right state
  LetStmt name maybeType expr -> do
    case typecheck' typeEnv expr maybeType of
      Left err -> return $ Left $ "Type error: " ++ show err
      Right (ty, _) -> do
        case eval globalEnv expr of
          Left err -> return $ Left $ "Runtime error: " ++ show err
          Right val -> do
            putStrLn $ "Type: " ++ show ty
            putStrLn $ "Value: " ++ show val
            let newTypeEnv = Map.insert name (generalize typeEnv ty) typeEnv
            let newGlobalEnv = (name, val) : globalEnv
            return $
              Right $
                state
                  { typeEnv = newTypeEnv,
                    globalEnv = newGlobalEnv
                  }
