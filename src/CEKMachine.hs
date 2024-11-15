{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module CEKMachine (eval, evalWithPrimitives, primitives, Value (..), GlobalEnv) where

import Data.ByteString.Char8 (ByteString)
import Syntax

type LocalEnv = [(ByteString, Value)]
type GlobalEnv = [(ByteString, Value)]

data PrimInfo = PrimInfo
  { arity :: Int,
    impl :: [Value] -> Maybe Value
  }

data Value
  = VUnit
  | VInt Int
  | VBool Bool
  | VClos Exp LocalEnv
  | VPair Value Value
  | VPrim PrimInfo
  | VRecClos ByteString (ByteString, Maybe Ty) Exp LocalEnv  -- 追加: 再帰関数用のコンストラクタ

instance Show Value where
  show VUnit = "()"
  show (VInt n) = show n
  show (VBool b) = show b
  show (VClos _ _) = "<closure>"
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show (VPrim _) = "<primitive>"
  show (VRecClos {}) = "<recursive closure>"

instance Eq Value where
  VUnit == VUnit = True
  VInt n1 == VInt n2 = n1 == n2
  VBool b1 == VBool b2 = b1 == b2
  VPair v1 v2 == VPair v3 v4 = v1 == v3 && v2 == v4
  _ == _ = False

data Cont
  = FunK Exp LocalEnv Cont
  | ArgK Value LocalEnv Cont
  | LetK ByteString Exp LocalEnv Cont
  | IfK Exp Exp LocalEnv Cont
  | PairK1 Exp LocalEnv Cont
  | PairK2 Value Cont
  | FstK Cont
  | SndK Cont
  | HaltK
  deriving (Eq, Show)

data State
  = Run Exp LocalEnv GlobalEnv Cont
  | Done Value
  deriving (Eq, Show)

primitives :: GlobalEnv
primitives =
  [ ("add", VPrim $ PrimInfo 2 addPrim),
    ("sub", VPrim $ PrimInfo 2 subPrim),
    ("mul", VPrim $ PrimInfo 2 mulPrim),
    ("div", VPrim $ PrimInfo 2 divPrim),
    ("neg", VPrim $ PrimInfo 1 negPrim),
    ("eq", VPrim $ PrimInfo 2 eqPrim),
    ("lt", VPrim $ PrimInfo 2 ltPrim),
    ("gt", VPrim $ PrimInfo 2 gtPrim),
    ("le", VPrim $ PrimInfo 2 lePrim),
    ("ge", VPrim $ PrimInfo 2 gePrim),
    ("ne", VPrim $ PrimInfo 2 nePrim)
  ]
  where
    addPrim [VInt x, VInt y] = Just $ VInt (x + y)
    addPrim _ = Nothing
    subPrim [VInt x, VInt y] = Just $ VInt (x - y)
    subPrim _ = Nothing
    mulPrim [VInt x, VInt y] = Just $ VInt (x * y)
    mulPrim _ = Nothing
    divPrim [VInt x, VInt y]
      | y /= 0 = Just $ VInt (x `div` y)
      | otherwise = Nothing
    divPrim _ = Nothing
    negPrim [VInt x] = Just $ VInt (-x)
    negPrim _ = Nothing
    eqPrim [VInt x, VInt y] = Just $ VBool (x == y)
    eqPrim [VBool x, VBool y] = Just $ VBool (x == y)
    eqPrim _ = Nothing
    ltPrim [VInt x, VInt y] = Just $ VBool (x < y)
    ltPrim _ = Nothing
    gtPrim [VInt x, VInt y] = Just $ VBool (x > y)
    gtPrim _ = Nothing
    lePrim [VInt x, VInt y] = Just $ VBool (x <= y)
    lePrim _ = Nothing
    gePrim [VInt x, VInt y] = Just $ VBool (x >= y)
    gePrim _ = Nothing
    nePrim [VInt x, VInt y] = Just $ VBool (x /= y)
    nePrim [VBool x, VBool y] = Just $ VBool (x /= y)
    nePrim _ = Nothing

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
  Unit -> applyK VUnit k g  -- 追加: ユニット値の評価
  Int n -> applyK (VInt n) k g
  Bool b -> applyK (VBool b) k g
  Fun (_, _) _ -> applyK (VClos c e) k g
  App e1 e2 -> Just $ Run e1 e g (FunK e2 e k)
  Let (Just x) _ e1 e2 -> Just $ Run e1 e g (LetK x e2 e k)
  Let Nothing _ e1 e2 -> Just $ Run e1 e g (LetK "$DUMMY" e2 e k)
  LetRec f (param, _) _ body expr ->  -- 追加: 再帰関数の評価
    let recClos = VRecClos f (param, Nothing) body e
    in Just $ Run expr ((f, recClos) : e) g k
  If e1 e2 e3 -> Just $ Run e1 e g (IfK e2 e3 e k)
  Pair e1 e2 -> Just $ Run e1 e g (PairK1 e2 e k)
  Fst c' -> Just $ Run c' e g (FstK k)
  Snd c' -> Just $ Run c' e g (SndK k)

applyK :: Value -> Cont -> GlobalEnv -> Maybe State
applyK val k g = case k of
  HaltK -> Just $ Done val
  FunK arg e k' -> Just $ Run arg e g (ArgK val e k')
  ArgK fun _ k' -> case fun of
    VClos (Fun (x, _) body) funE ->
      Just $ Run body ((x, val) : funE) g k'
    VRecClos f (param, _) body funE ->  -- 追加: 再帰関数呼び出しの処理
      let recClos = VRecClos f (param, Nothing) body funE
          newEnv = (f, recClos) : (param, val) : funE
      in Just $ Run body newEnv g k'
    VPrim (PrimInfo {..}) ->
      if arity > 1
        then applyK (VPrim $ PrimInfo (arity - 1) (\args -> impl (val : args))) k' g
        else case impl [val] of
          Just result -> applyK result k' g
          Nothing -> Nothing
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

eval :: GlobalEnv -> Exp -> Maybe Value
eval g c = go (Run c [] g HaltK)
  where
    go state = case step state of
      Nothing -> case state of
        Done val -> Just val
        _ -> Nothing
      Just state' -> go state'

evalWithPrimitives :: Exp -> Maybe Value
evalWithPrimitives = eval primitives
