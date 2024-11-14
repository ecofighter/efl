{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module CAMachine
  ( eval,
    evalWithPrimitives,
    primitives,
    Value (..),
    GlobalEnv,
    CAMError (..),
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.List (elemIndex)
import GHC.Generics
import Syntax

data Value
  = VInt Int
  | VBool Bool
  | VClosure Code [Value]
  | VPair Value Value
  | VPrim PrimInfo
  deriving (Generic)

instance Show Value where
  show = \case
    VInt n -> show n
    VBool b -> show b
    VClosure _ _ -> "<closure>"
    VPair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
    VPrim _ -> "<primitive>"

instance Eq Value where
  VInt n1 == VInt n2 = n1 == n2
  VBool b1 == VBool b2 = b1 == b2
  VPair v1 v2 == VPair v3 v4 = v1 == v3 && v2 == v4
  VClosure c1 e1 == VClosure c2 e2 = c1 == c2 && e1 == e2
  VPrim p1 == VPrim p2 = p1 == p2
  _ == _ = False

data PrimInfo = PrimInfo
  { primName :: ByteString,
    arity :: Int,
    impl :: [Value] -> Either String Value,
    argStack :: [Value]
  }

instance Eq PrimInfo where
  PrimInfo n1 a1 _ _ == PrimInfo n2 a2 _ _ = n1 == n2 && a1 == a2

type Env = [Value]

type GlobalEnv = [(ByteString, Value)]

data CAMError
  = StackUnderflow String
  | TypeError String
  | RuntimeError String
  | UnboundVariable ByteString
  | DivisionByZero
  deriving (Show, Eq)

data Instr
  = Access Int
  | Closure Code
  | Apply
  | Return
  | Push
  | Quote Value
  | Cons
  | Car
  | Cdr
  | Test Code Code
  | Let
  | EndLet
  | Global ByteString
  deriving (Generic, Eq, Show)

type Code = [Instr]

data State = State
  { code :: Code,
    env :: Env,
    stack :: [Value],
    dump :: [(Code, Env, [Value])],
    globals :: GlobalEnv
  }

eval :: GlobalEnv -> Exp -> Either CAMError Value
eval globals expr = do
  finalState <- evalState $ State (compile expr) [] [] [] globals
  case stack finalState of
    (v : _) -> Right v
    [] -> Left $ RuntimeError "Empty stack after evaluation"

evalWithPrimitives :: Exp -> Either CAMError Value
evalWithPrimitives = eval primitives

evalState :: State -> Either CAMError State
evalState s@State {..} = case code of
  [] -> Right s
  (i : is) -> do
    s' <- step s {code = is} i
    evalState s'

step :: State -> Instr -> Either CAMError State
step s@State {..} = \case
  Access n
    | n < length env -> Right s {stack = env !! n : stack}
    | otherwise -> Left $ RuntimeError $ "Invalid environment access: " ++ show n
  Global name -> case lookup name globals of
    Just v -> Right s {stack = v : stack}
    Nothing -> Left $ UnboundVariable name
  Closure c -> Right s {stack = VClosure c env : stack}
  Apply -> case stack of
    (v : VClosure c env' : rest) ->
      Right State
        { code = c,
          env = v : env',
          stack = [],
          dump = (code, env, rest) : dump,
          globals = globals
        }
    (v : VPrim prim@PrimInfo {..} : rest) ->
      let newArgStack = v : argStack
          newPrim = prim {argStack = newArgStack}
       in if length newArgStack == arity
            then case impl (reverse newArgStack) of
              Right result -> Right s {stack = result : rest}
              Left err -> Left $ RuntimeError err
            else Right s {stack = VPrim newPrim : rest}
    _ -> Left $ TypeError "Invalid application"
  Return -> case dump of
    ((code', env', stack') : dump') ->
      case stack of
        (v : _) -> Right State
          { code = code',
            env = env',
            stack = v : stack',
            dump = dump',
            globals = globals
          }
        [] -> Left $ StackUnderflow "Return"
    [] -> Left $ RuntimeError "Return from top-level"
  Quote v -> Right s {stack = v : stack}
  Push -> case stack of
    (v : rest) -> Right s {stack = v : v : rest}
    [] -> Left $ StackUnderflow "Push"
  Cons -> case stack of
    (v2 : v1 : rest) -> Right s {stack = VPair v1 v2 : rest}
    _ -> Left $ StackUnderflow "Cons"
  Car -> case stack of
    (VPair v1 _ : rest) -> Right s {stack = v1 : rest}
    _ -> Left $ TypeError "Car: Expected pair"
  Cdr -> case stack of
    (VPair _ v2 : rest) -> Right s {stack = v2 : rest}
    _ -> Left $ TypeError "Cdr: Expected pair"
  Test c1 c2 -> case stack of
    (VBool b : rest) ->
      Right s
        { code = (if b then c1 else c2) ++ code,
          stack = rest
        }
    _ -> Left $ TypeError "Test: Expected boolean"
  CAMachine.Let -> case stack of
    (v : _) -> Right s {env = v : env}
    [] -> Left $ StackUnderflow "Let"
  EndLet -> case env of
    (_ : env') -> Right s {env = env'}
    [] -> Left $ RuntimeError "EndLet: Empty environment"

compile :: Exp -> Code
compile = compileWithEnv []

compileWithEnv :: [ByteString] -> Exp -> Code
compileWithEnv env = \case
  Var x -> case elemIndex x env of
    Just i -> [Access i]
    Nothing -> [Global x]
  Int n -> [Quote (VInt n)]
  Bool b -> [Quote (VBool b)]
  Fun (x, _) body ->
    [Closure (compileWithEnv (x : env) body ++ [Return])]
  App e1 e2 ->
    compileWithEnv env e1 ++ compileWithEnv env e2 ++ [Apply]
  Syntax.Let x _ e1 e2 ->
    compileWithEnv env e1
      ++ [CAMachine.Let]
      ++ compileWithEnv (x : env) e2
      ++ [EndLet]
  If e1 e2 e3 ->
    compileWithEnv env e1 ++ [Test (compileWithEnv env e2) (compileWithEnv env e3)]
  Pair e1 e2 ->
    compileWithEnv env e1 ++ [Push] ++ compileWithEnv env e2 ++ [Cons]
  Fst e -> compileWithEnv env e ++ [Car]
  Snd e -> compileWithEnv env e ++ [Cdr]

primitives :: GlobalEnv
primitives =
  [ ("add", makePrim "add" 2 addImpl),
    ("sub", makePrim "sub" 2 subImpl),
    ("mul", makePrim "mul" 2 mulImpl),
    ("div", makePrim "div" 2 divImpl),
    ("neg", makePrim "neg" 1 negImpl),
    ("eq", makePrim "eq" 2 eqImpl),
    ("lt", makePrim "lt" 2 ltImpl),
    ("gt", makePrim "gt" 2 gtImpl),
    ("le", makePrim "le" 2 leImpl),
    ("ge", makePrim "ge" 2 geImpl),
    ("ne", makePrim "ne" 2 neImpl)
  ]
  where
    makePrim name arity impl = VPrim $ PrimInfo name arity impl []

    addImpl [VInt x, VInt y] = Right $ VInt (x + y)
    addImpl _ = Left "Type error in addition"

    subImpl [VInt x, VInt y] = Right $ VInt (x - y)
    subImpl _ = Left "Type error in subtraction"

    mulImpl [VInt x, VInt y] = Right $ VInt (x * y)
    mulImpl _ = Left "Type error in multiplication"

    divImpl [VInt x, VInt y]
      | y == 0 = Left "Division by zero"
      | otherwise = Right $ VInt (x `div` y)
    divImpl _ = Left "Type error in division"

    negImpl [VInt x] = Right $ VInt (-x)
    negImpl _ = Left "Type error in negation"

    eqImpl [VInt x, VInt y] = Right $ VBool (x == y)
    eqImpl [VBool x, VBool y] = Right $ VBool (x == y)
    eqImpl _ = Left "Type error in equality comparison"

    ltImpl [VInt x, VInt y] = Right $ VBool (x < y)
    ltImpl _ = Left "Type error in less than comparison"

    gtImpl [VInt x, VInt y] = Right $ VBool (x > y)
    gtImpl _ = Left "Type error in greater than comparison"

    leImpl [VInt x, VInt y] = Right $ VBool (x <= y)
    leImpl _ = Left "Type error in less than or equal comparison"

    geImpl [VInt x, VInt y] = Right $ VBool (x >= y)
    geImpl _ = Left "Type error in greater than or equal comparison"

    neImpl [VInt x, VInt y] = Right $ VBool (x /= y)
    neImpl [VBool x, VBool y] = Right $ VBool (x /= y)
    neImpl _ = Left "Type error in not equal comparison"
