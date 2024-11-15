{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import CAMachine
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map qualified as Map
import Parser
import Syntax
import System.Console.Haskeline
import Text.Trifecta
import TypeChecker

data Command
  = Eval ByteString
  | TypeOf ByteString
  | Help
  | Quit
  deriving (Show)

data InterpreterState = InterpreterState
  { globals :: GlobalEnv,
    typeEnv :: TypeEnv
  }

initialState :: InterpreterState
initialState =
  InterpreterState
    { globals = primitives,
      typeEnv = primitivesTypes
    }

parseCommand :: ByteString -> Maybe Command
parseCommand input = case BS.words input of
  [":t", expr] -> Just $ TypeOf (BS.unwords ["let _ =", expr])
  [":h"] -> Just Help
  [":q"] -> Just Quit
  _ -> Just $ Eval input

showType :: Ty -> String
showType TyInt = "Int"
showType TyBool = "Bool"
showType TyUnit = "Unit"
showType (TyArr t1 t2) = "(" ++ showType t1 ++ " -> " ++ showType t2 ++ ")"
showType (TyProd t1 t2) = "(" ++ showType t1 ++ ", " ++ showType t2 ++ ")"
showType (TyVar n) = "t" ++ show n

processCommand :: InterpreterState -> Command -> InputT IO (Maybe InterpreterState)
processCommand state@InterpreterState {..} cmd = case cmd of
  Help -> do
    outputStrLn "Available commands:"
    outputStrLn "  <expr>     Evaluate expression"
    outputStrLn "  :t <expr>  Show type of expression"
    outputStrLn "  :h         Show this help"
    outputStrLn "  :q         Quit"
    return $ Just state
  Quit -> return Nothing
  TypeOf src -> do
    case parseString parseStmt mempty (BS.unpack src) of
      Success stmt -> case stmt of
        LetStmt _ _ expr -> case typecheck' typeEnv expr Nothing of
          Right (ty, _) -> outputStrLn $ showType ty
          Left e -> outputStrLn $ "Type error: " ++ show e
        LetRecStmt name param retTy body ->
          let expr = LetRec name param retTy body (Var name)
           in case typecheck' typeEnv expr Nothing of
                Right (ty, _) -> outputStrLn $ showType ty
                Left e -> outputStrLn $ "Type error: " ++ show e
      Failure e -> outputStrLn $ "Parse error: " ++ show e
    return $ Just state
  Eval src -> do
    case parseString parseStmt mempty (BS.unpack src) of
      Success stmt -> case stmt of
        LetStmt name annotTy expr -> case typecheck' typeEnv expr annotTy of
          Right (ty, _) -> case eval globals expr of
            Right val -> do
              outputStrLn $ show val ++ " : " ++ showType ty
              case name of
                Just n -> do
                  let scheme = generalize typeEnv ty
                  return $
                    Just $
                      state
                        { globals = (n, val) : globals,
                          typeEnv = Map.insert n scheme typeEnv
                        }
                Nothing -> return $ Just state
            Left e -> do
              outputStrLn $ "Runtime error: " ++ show e
              return $ Just state
          Left e -> do
            outputStrLn $ "Type error: " ++ show e
            return $ Just state
        LetRecStmt name (param, _) retTy body ->
          let actualRetTy = case retTy of
                Just (TyArr _ ret) -> Just ret
                _ -> Nothing
              expr = LetRec name (param, Nothing) actualRetTy body (Var name)
           in case typecheck' typeEnv expr Nothing of
                Right (ty, _) -> case eval globals expr of
                  Right val -> do
                    outputStrLn $ show val ++ " : " ++ showType ty
                    let scheme = generalize typeEnv ty
                    return $
                      Just $
                        state
                          { globals = (name, val) : globals,
                            typeEnv = Map.insert name scheme typeEnv
                          }
                  Left e -> do
                    outputStrLn $ "Runtime error: " ++ show e
                    return $ Just state
                Left e -> do
                  outputStrLn $ "Type error: " ++ show e
                  return $ Just state
      Failure e -> do
        outputStrLn $ "Parse error: " ++ show e
        return $ Just state

repl :: IO ()
repl = runInputT defaultSettings $ do
  outputStrLn "Simple ML Interpreter"
  outputStrLn "Type :h for help"
  loop initialState
  where
    loop state = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> case parseCommand (BS.pack input) of
          Just cmd -> do
            mstate' <- processCommand state cmd
            forM_ mstate' loop
          Nothing -> do
            outputStrLn "Invalid command"
            loop state

main :: IO ()
main = repl
