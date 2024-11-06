{-# LANGUAGE StrictData #-}

module CEKMachine (eval, Value(..), GlobalEnv) where

import Data.ByteString.Char8 (ByteString)
import Syntax

type LocalEnv = [(ByteString, Value)]
type GlobalEnv = [(ByteString, Value)]

data Value
  = VInt Int
  | VBool Bool
  | VClos Exp LocalEnv
  deriving (Eq, Show)

data Cont
  = FunK Exp LocalEnv Cont
  | ArgK Value LocalEnv Cont
  | LetK ByteString Exp LocalEnv Cont
  | IfK Exp Exp LocalEnv Cont
  | HaltK
  deriving (Eq, Show)

data State
  = Run Exp LocalEnv GlobalEnv Cont
  | Done Value
  deriving (Eq, Show)

-- 変数を探す際は、まずローカル環境を確認し、見つからなければグローバル環境を確認する
lookupVar :: ByteString -> LocalEnv -> GlobalEnv -> Maybe Value
lookupVar x localEnv globalEnv =
  case lookup x localEnv of
    Just val -> Just val
    Nothing -> lookup x globalEnv

step :: State -> Maybe State
step (Done _) = Nothing
step (Run c e g k) = case c of
  Var x -> case lookupVar x e g of
    Just val -> applyK val k g
    Nothing -> Nothing
  Int n -> applyK (VInt n) k g
  Bool b -> applyK (VBool b) k g
  Fun (_, _) _ -> applyK (VClos c e) k g
  App e1 e2 -> Just $ Run e1 e g (FunK e2 e k)
  Let x _ e1 e2 -> Just $ Run e1 e g (LetK x e2 e k)
  If e1 e2 e3 -> Just $ Run e1 e g (IfK e2 e3 e k)

applyK :: Value -> Cont -> GlobalEnv -> Maybe State
applyK val k g = case k of
  HaltK -> Just $ Done val
  FunK arg e k' -> Just $ Run arg e g (ArgK val e k')
  ArgK fun _ k' -> case fun of
    VClos (Fun (x, _) body) funE ->
      Just $ Run body ((x, val) : funE) g k'
    _ -> Nothing
  LetK x body e k' ->
    Just $ Run body ((x, val) : e) g k'
  IfK e2 e3 e k' -> case val of
    VBool True -> Just $ Run e2 e g k'
    VBool False -> Just $ Run e3 e g k'
    _ -> Nothing

-- 評価関数はグローバル環境を引数として受け取るように変更
eval :: GlobalEnv -> Exp -> Maybe Value
eval g c = go (Run c [] g HaltK)
  where
    go state = case step state of
      Nothing -> case state of
        Done val -> Just val
        _ -> Nothing
      Just state' -> go state'
