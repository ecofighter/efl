{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker
  ( typeCheck,
    typeCheckStmt,
    initialTypeEnv,
    TypeError (..),
    TypeEnv,
    Substitution,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Syntax

-- 型エラーを表す型
data TypeError
  = UnificationFailed Ty Ty
  | UnboundVariable ByteString
  | OccursCheckFailed Int Ty
  | TypeMismatch String Ty Ty
  deriving (Show, Eq)

-- 型環境
type TypeEnv = Map ByteString Ty

-- 型代入
type Substitution = Map Int Ty

-- 型検査の状態
data TypeState = TypeState
  { typeCounter :: Int, -- フレッシュな型変数を生成するためのカウンター
    typeSubst :: Substitution -- 現在の型代入
  }

-- 型検査モナド
type TypeCheck a = ExceptT TypeError (State TypeState) a

-- 初期状態
initialTypeState :: TypeState
initialTypeState =
  TypeState
    { typeCounter = 0,
      typeSubst = Map.empty
    }

-- TypeCheckモナドを評価
runTypeCheck :: TypeCheck a -> Either TypeError a
runTypeCheck m = evalState (runExceptT m) initialTypeState

-- フレッシュな型変数を生成
freshTyVar :: TypeCheck Ty
freshTyVar = do
  counter <- gets typeCounter
  modify $ \s -> s {typeCounter = counter + 1}
  return $ TyVar counter

-- 型の正規化（代入を適用）
normalize :: Ty -> TypeCheck Ty
normalize ty = do
  subst <- gets typeSubst
  return $ applySubst subst ty
  where
    applySubst :: Substitution -> Ty -> Ty
    applySubst s = \case
      TyVar n -> case Map.lookup n s of
        Just t -> applySubst s t
        Nothing -> TyVar n
      TyArr t1 t2 -> TyArr (applySubst s t1) (applySubst s t2)
      TyProd t1 t2 -> TyProd (applySubst s t1) (applySubst s t2)
      t -> t

-- 単一化アルゴリズム
unify :: Ty -> Ty -> TypeCheck ()
unify t1 t2 = do
  t1' <- normalize t1
  t2' <- normalize t2
  unify' t1' t2'
  where
    unify' :: Ty -> Ty -> TypeCheck ()
    unify' (TyVar n) t = bindTyVar n t
    unify' t (TyVar n) = bindTyVar n t
    unify' TyUnit TyUnit = return ()
    unify' TyInt TyInt = return ()
    unify' TyBool TyBool = return ()
    unify' (TyArr a1 b1) (TyArr a2 b2) = do
      unify a1 a2
      unify b1 b2
    unify' (TyProd a1 b1) (TyProd a2 b2) = do
      unify a1 a2
      unify b1 b2
    unify' t1' t2' = throwError $ UnificationFailed t1' t2'

    bindTyVar :: Int -> Ty -> TypeCheck ()
    bindTyVar n t = do
      if occursCheck n t
        then throwError $ OccursCheckFailed n t
        else modify $ \s -> s {typeSubst = Map.insert n t (typeSubst s)}

    occursCheck :: Int -> Ty -> Bool
    occursCheck n = \case
      TyVar m -> n == m
      TyArr t1' t2' -> occursCheck n t1' || occursCheck n t2'
      TyProd t1' t2' -> occursCheck n t1' || occursCheck n t2'
      _ -> False

-- 式の型検査
inferType :: TypeEnv -> Exp -> TypeCheck Ty
inferType env = \case
  Unit -> return TyUnit
  Int _ -> return TyInt
  Bool _ -> return TyBool
  Var x -> case Map.lookup x env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable x
  Fun (x, paramTy) body -> do
    bodyTy <- inferType (Map.insert x paramTy env) body
    return $ TyArr paramTy bodyTy
  App e1 e2 -> do
    funTy <- inferType env e1
    argTy <- inferType env e2
    funTy' <- normalize funTy
    case funTy' of
      TyArr paramTy resultTy -> do
        unify paramTy argTy
        return resultTy
      _ -> do
        resultTy <- freshTyVar
        unify funTy' (TyArr argTy resultTy)
        return resultTy
  Let mname ty e1 e2 -> do
    t1 <- inferType env e1
    unify ty t1
    case mname of
      Just name -> inferType (Map.insert name ty env) e2
      Nothing -> inferType env e2
  LetRec name (param, paramTy) ty body expr -> do
    let env' = Map.insert name ty env
    let env'' = Map.insert param paramTy env'
    bodyTy <- inferType env'' body
    unify ty (TyArr paramTy bodyTy)
    inferType env' expr
  If cond then_ else_ -> do
    condTy <- inferType env cond
    unify condTy TyBool
    thenTy <- inferType env then_
    elseTy <- inferType env else_
    unify thenTy elseTy
    normalize thenTy
  Pair e1 e2 -> do
    t1 <- inferType env e1
    t2 <- inferType env e2
    return $ TyProd t1 t2
  Fst e -> do
    ty <- inferType env e
    a <- freshTyVar
    b <- freshTyVar
    unify ty (TyProd a b)
    normalize a
  Snd e -> do
    ty <- inferType env e
    a <- freshTyVar
    b <- freshTyVar
    unify ty (TyProd a b)
    normalize b

-- 文の型検査
typeCheckStmt :: TypeEnv -> Stmt -> Either TypeError (TypeEnv, Ty)
typeCheckStmt env stmt = runTypeCheck $ case stmt of
  LetStmt mname expectedTy e -> do
    inferredTy <- inferType env e
    unify expectedTy inferredTy
    normalizedTy <- normalize expectedTy
    case mname of
      Just name -> return (Map.insert name normalizedTy env, normalizedTy)
      Nothing -> return (env, normalizedTy)
  LetRecStmt name (param, paramTy) expectedTy body -> do
    let env' = Map.insert name expectedTy env
    let env'' = Map.insert param paramTy env'
    bodyTy <- inferType env'' body
    unify expectedTy (TyArr paramTy bodyTy)
    normalizedTy <- normalize expectedTy
    return (Map.insert name normalizedTy env, normalizedTy)

-- 式の型検査（外部向けインターフェース）
typeCheck :: TypeEnv -> Exp -> Either TypeError Ty
typeCheck env expr = runTypeCheck $ do
  ty <- inferType env expr
  normalize ty

initialTypeEnv :: TypeEnv
initialTypeEnv =
  Map.fromList
    [ ("add", TyArr TyInt (TyArr TyInt TyInt)),
      ("sub", TyArr TyInt (TyArr TyInt TyInt)),
      ("mul", TyArr TyInt (TyArr TyInt TyInt)),
      ("div", TyArr TyInt (TyArr TyInt TyInt)),
      ("eq", TyArr TyInt (TyArr TyInt TyBool)),
      ("lt", TyArr TyInt (TyArr TyInt TyBool)),
      ("gt", TyArr TyInt (TyArr TyInt TyBool)),
      ("le", TyArr TyInt (TyArr TyInt TyBool)),
      ("ge", TyArr TyInt (TyArr TyInt TyBool)),
      ("ne", TyArr TyInt (TyArr TyInt TyBool)),
      ("neg", TyArr TyInt TyInt)
    ]
