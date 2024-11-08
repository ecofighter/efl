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
  | VPair Value Value
  deriving (Eq, Show)

data Cont
  = FunK Exp LocalEnv Cont
  | ArgK Value LocalEnv Cont
  | LetK ByteString Exp LocalEnv Cont
  | IfK Exp Exp LocalEnv Cont
  | PairK1 Exp LocalEnv Cont  -- 追加: ペアの第1要素評価用
  | PairK2 Value Cont         -- 追加: ペアの第2要素評価用
  | FstK Cont                 -- 追加: 第1要素取得用
  | SndK Cont                 -- 追加: 第2要素取得用
  | MatchK [(Pattern, Exp)] LocalEnv Cont
  | HaltK
  deriving (Eq, Show)

data State
  = Run Exp LocalEnv GlobalEnv Cont
  | Done Value
  deriving (Eq, Show)

type MatchResult = Maybe LocalEnv

-- パターンマッチを行う関数
matchPattern :: Pattern -> Value -> MatchResult
matchPattern (PVar x) val = Just [(x, val)]
matchPattern (PPair p1 p2) (VPair v1 v2) = do
  env1 <- matchPattern p1 v1
  env2 <- matchPattern p2 v2
  return $ env1 ++ env2
matchPattern _ _ = Nothing

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
  Pair e1 e2 -> Just $ Run e1 e g (PairK1 e2 e k)
  Fst c' -> Just $ Run c' e g (FstK k)
  Snd c' -> Just $ Run c' e g (SndK k)
  Match c' cases -> Just $ Run c' e g (MatchK cases e k)
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
  PairK1 e2 e k' -> Just $ Run e2 e g (PairK2 val k')
  PairK2 v1 k' -> applyK (VPair v1 val) k' g
  FstK k' -> case val of
    VPair v1 _ -> applyK v1 k' g
    _ -> Nothing
  SndK k' -> case val of
    VPair _ v2 -> applyK v2 k' g
    _ -> Nothing
  MatchK cases env k' ->
    case findMatch val cases env of
      Just (expr, newEnv) -> Just $ Run expr newEnv g k'
      Nothing -> Nothing  -- パターンマッチ失敗

findMatch :: Value -> [(Pattern, Exp)] -> LocalEnv -> Maybe (Exp, LocalEnv)
findMatch _ [] _ = Nothing
findMatch val ((pat, c):rest) env =
  case matchPattern pat val of
    Just newBindings -> Just (c, newBindings ++ env)
    Nothing -> findMatch val rest env

-- 評価関数はグローバル環境を引数として受け取るように変更
eval :: GlobalEnv -> Exp -> Maybe Value
eval g c = go (Run c [] g HaltK)
  where
    go state = case step state of
      Nothing -> case state of
        Done val -> Just val
        _ -> Nothing
      Just state' -> go state'
