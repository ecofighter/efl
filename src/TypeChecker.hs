{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module TypeChecker
  ( TyEnv,
    TypeCheckError (..),
    runTypeCheck,
    inferExp,
    inferStmt,
    checkExp,
    checkStmt,
    initialEnv,
    generalize,
    instantiate,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Syntax

-- 型環境：変数名から型スキームへのマッピング
type TyEnv = Map ByteString TyScheme

-- 型検査エラー
data TypeCheckError
  = UnificationError Ty Ty
  | UnboundVariable ByteString
  | OccursCheck Int Ty
  deriving (Show, Eq)

-- 型検査の状態：フレッシュな型変数を生成するためのカウンタと
-- 単一化で生成される型代入を保持
data TCState = TCState
  { counter :: Int,
    subst :: IntMap Ty
  }
  deriving (Show, Eq)

-- 型検査モナド
type TC a = ExceptT TypeCheckError (State TCState) a

-- フレッシュな型変数を生成
freshTyVar :: TC Ty
freshTyVar = do
  s <- get
  let c = counter s
  put s {counter = c + 1}
  return $ TyVar c

-- 型代入を適用
applySubst :: Ty -> TC Ty
applySubst ty = do
  s <- gets subst
  return $ go s ty
  where
    go s = \case
      TyVar n -> case IntMap.lookup n s of
        Just t | t /= TyVar n -> go s t
        _ -> TyVar n
      TyArr t1 t2 -> TyArr (go s t1) (go s t2)
      TyProd t1 t2 -> TyProd (go s t1) (go s t2)
      t -> t

-- 出現検査
occursCheck :: Int -> Ty -> TC ()
occursCheck n ty = do
  ty' <- applySubst ty
  case ty' of
    TyVar m | m == n -> return ()
    _ -> when (n `IntSet.member` freeVars ty' && ty' /= TyVar n) $
          throwError $ OccursCheck n ty
  where
    freeVars = \case
      TyVar m -> IntSet.singleton m
      TyArr t1 t2 -> freeVars t1 `IntSet.union` freeVars t2
      TyProd t1 t2 -> freeVars t1 `IntSet.union` freeVars t2
      _ -> IntSet.empty

-- 単一化
unify :: Ty -> Ty -> TC ()
unify t1 t2 = do
  t1' <- applySubst t1
  t2' <- applySubst t2
  case (t1', t2') of
    _ | t1' == t2' -> return ()
    (TyVar n, t) -> unifyVar n t
    (t, TyVar n) -> unifyVar n t
    (TyUnit, TyUnit) -> return ()
    (TyInt, TyInt) -> return ()
    (TyBool, TyBool) -> return ()
    (TyArr a1 a2, TyArr b1 b2) -> do
      unify a1 b1
      unify a2 b2
    (TyProd a1 a2, TyProd b1 b2) -> do
      unify a1 b1
      unify a2 b2
    _ -> throwError $ UnificationError t1' t2'
  where
    unifyVar n t = do
      occursCheck n t
      modify $ \s -> s {subst = IntMap.insert n t (subst s)}

-- 型スキームの一般化
generalize :: TyEnv -> Ty -> TC TyScheme
generalize env ty = do
  ty' <- applySubst ty
  let envVars = IntSet.unions $ map freeVarsScheme $ Map.elems env
      freeVars = typeVars ty' `IntSet.difference` envVars
  return $ TyScheme freeVars ty'
  where
    typeVars = \case
      TyVar n -> IntSet.singleton n
      TyArr t1 t2 -> typeVars t1 `IntSet.union` typeVars t2
      TyProd t1 t2 -> typeVars t1 `IntSet.union` typeVars t2
      _ -> IntSet.empty
    freeVarsScheme (TyScheme vs t) =
      typeVars t `IntSet.difference` vs

-- 型スキームのインスタンス化
instantiate :: TyScheme -> TC Ty
instantiate (TyScheme vars ty) = do
  subst <- forM (IntSet.toList vars) $ \v -> do
    fresh <- freshTyVar
    return (v, fresh)
  return $ substTy (IntMap.fromList subst) ty
  where
    substTy s = \case
      TyVar n -> case IntMap.lookup n s of
        Just t -> t
        Nothing -> TyVar n
      TyArr t1 t2 -> TyArr (substTy s t1) (substTy s t2)
      TyProd t1 t2 -> TyProd (substTy s t1) (substTy s t2)
      t -> t

-- 式の型推論
inferExp :: TyEnv -> Exp -> TC Ty
inferExp env = \case
  Var x -> case Map.lookup x env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ UnboundVariable x
  Unit -> return TyUnit
  Int _ -> return TyInt
  Bool _ -> return TyBool
  Fun (x, paramTy) body -> do
    bodyTy <- inferExp (Map.insert x (TyScheme IntSet.empty paramTy) env) body
    return $ TyArr paramTy bodyTy
  App e1 e2 -> do
    funTy <- inferExp env e1
    argTy <- inferExp env e2
    retTy <- freshTyVar
    unify funTy (TyArr argTy retTy)
    return retTy
  Let mname ty e1 e2 -> do
    t1 <- inferExp env e1
    unify ty t1
    case mname of
      Nothing -> inferExp env e2
      Just x -> do
        scheme <- generalize env ty
        inferExp (Map.insert x scheme env) e2
  LetRec f (x, paramTy) retTy body expr -> do
    let funTy = TyArr paramTy retTy
        fScheme = TyScheme IntSet.empty funTy
        bodyEnv =
          Map.insert x (TyScheme IntSet.empty paramTy) $
            Map.insert f fScheme env
    bodyTy <- inferExp bodyEnv body
    unify retTy bodyTy
    scheme <- generalize env funTy
    inferExp (Map.insert f scheme env) expr
  If cond e1 e2 -> do
    condTy <- inferExp env cond
    t1 <- inferExp env e1
    t2 <- inferExp env e2
    unify condTy TyBool
    unify t1 t2
    applySubst t1
  Pair e1 e2 -> do
    t1 <- inferExp env e1
    t2 <- inferExp env e2
    return $ TyProd t1 t2
  Fst e -> do
    ty <- inferExp env e
    t1 <- freshTyVar
    t2 <- freshTyVar
    unify ty (TyProd t1 t2)
    return t1
  Snd e -> do
    ty <- inferExp env e
    t1 <- freshTyVar
    t2 <- freshTyVar
    unify ty (TyProd t1 t2)
    return t2

-- 文の型推論
inferStmt :: TyEnv -> Stmt -> TC (TyEnv, Ty)
inferStmt env = \case
  LetStmt Nothing ty e -> do
    t <- inferExp env e
    unify ty t
    return (env, ty)
  LetStmt (Just x) ty e -> do
    t <- inferExp env e
    unify ty t
    scheme <- generalize env ty
    return (Map.insert x scheme env, ty)
  LetRecStmt f (x, paramTy) retTy body -> do
    let funTy = TyArr paramTy retTy
        fScheme = TyScheme IntSet.empty funTy
        bodyEnv =
          Map.insert x (TyScheme IntSet.empty paramTy) $
            Map.insert f fScheme env
    bodyTy <- inferExp bodyEnv body
    unify retTy bodyTy
    scheme <- generalize env funTy
    return (Map.insert f scheme env, funTy)

-- 型検査の実行
runTypeCheck :: TC a -> Either TypeCheckError a
runTypeCheck m = evalState (runExceptT m) (TCState 0 IntMap.empty)

-- 式の型検査
checkExp :: TyEnv -> Exp -> Either TypeCheckError Ty
checkExp env e = runTypeCheck $ do
  ty <- inferExp env e
  applySubst ty

-- 文の型検査
checkStmt :: TyEnv -> Stmt -> Either TypeCheckError (TyEnv, Ty)
checkStmt env s = runTypeCheck $ do
  (env', ty) <- inferStmt env s
  ty' <- applySubst ty
  return (env', ty')


-- 初期型環境
initialEnv :: TyEnv
initialEnv = Map.fromList
  [ -- 算術演算
    ("add", binaryOpScheme TyInt TyInt TyInt)
  , ("sub", binaryOpScheme TyInt TyInt TyInt)
  , ("mul", binaryOpScheme TyInt TyInt TyInt)
  , ("div", binaryOpScheme TyInt TyInt TyInt)
  , ("neg", monomorphicScheme $ TyArr TyInt TyInt)

    -- 比較演算（整数）
  , ("eq", binaryOpScheme TyInt TyInt TyBool)
  , ("ne", binaryOpScheme TyInt TyInt TyBool)
  , ("lt", binaryOpScheme TyInt TyInt TyBool)
  , ("gt", binaryOpScheme TyInt TyInt TyBool)
  , ("le", binaryOpScheme TyInt TyInt TyBool)
  , ("ge", binaryOpScheme TyInt TyInt TyBool)
  ]
  where
    -- 単相的な型を型スキームに変換
    monomorphicScheme :: Ty -> TyScheme
    monomorphicScheme = TyScheme IntSet.empty
    -- 二項演算の型スキーム
    binaryOpScheme :: Ty -> Ty -> Ty -> TyScheme
    binaryOpScheme t1 t2 t3 = monomorphicScheme $ TyArr t1 (TyArr t2 t3)

