{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker
  ( typecheck,
    typecheck',
    inferStmt,
    primitivesTypes,
    generalize,
    TypeError (..),
    Substitution,
    TyScheme (..),
    TypeEnv,
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Syntax

data TypeError
  = UnboundVariable ByteString
  | TypeMismatch Ty Ty
  | ExpectedFunction Ty
  | ExpectedBool Ty
  | OccursCheck Int Ty
  deriving (Show, Eq)

data TyScheme = Forall [Int] Ty
  deriving (Show, Eq)

type Substitution = Map Int Ty

type TypeEnv = Map ByteString TyScheme

data TyState = TyState
  { tyCounter :: Int,
    tySubst :: Substitution
  }

-- Helper functions
newTyVar :: (State TyState :> es) => Eff es Ty
newTyVar = do
  s <- get
  put s {tyCounter = tyCounter s + 1}
  return $ TyVar (tyCounter s)

freeTypeVars :: Ty -> Set.Set Int
freeTypeVars = \case
  TyVar n -> Set.singleton n
  TyArr t1 t2 -> freeTypeVars t1 `Set.union` freeTypeVars t2
  TyProd t1 t2 -> freeTypeVars t1 `Set.union` freeTypeVars t2
  _ -> Set.empty

freeTypeVarsScheme :: TyScheme -> Set.Set Int
freeTypeVarsScheme (Forall vars ty) =
  freeTypeVars ty `Set.difference` Set.fromList vars

freeTypeVarsEnv :: TypeEnv -> Set.Set Int
freeTypeVarsEnv env =
  Set.unions $ map freeTypeVarsScheme $ Map.elems env

class Substitutable a where
  apply :: Substitution -> a -> a
  ftv :: a -> Set.Set Int

instance Substitutable Ty where
  apply s ty@(TyVar n) = Map.findWithDefault ty n s
  apply s (TyArr t1 t2) = TyArr (apply s t1) (apply s t2)
  apply s (TyProd t1 t2) = TyProd (apply s t1) (apply s t2)
  apply _ ty = ty

  ftv = freeTypeVars

instance Substitutable TyScheme where
  apply s (Forall vars ty) = Forall vars (apply s' ty)
    where
      s' = foldr Map.delete s vars
  ftv = freeTypeVarsScheme

instance Substitutable TypeEnv where
  apply s = Map.map (apply s)
  ftv = freeTypeVarsEnv

-- 型の単一化
unify :: (State TyState :> es, Error TypeError :> es) => Ty -> Ty -> Eff es ()
unify t1 t2 = do
  s <- gets tySubst
  s' <- unifyWith s t1 t2
  modify $ \st -> st {tySubst = s'}

unifyWith :: (State TyState :> es, Error TypeError :> es) => Substitution -> Ty -> Ty -> Eff es Substitution
unifyWith s t1 t2 = case (apply s t1, apply s t2) of
  (TyVar v1, TyVar v2) | v1 == v2 -> return s
  (TyVar v, t) -> varBind v t
  (t, TyVar v) -> varBind v t
  (TyArr l1 r1, TyArr l2 r2) -> do
    s1 <- unifyWith s l1 l2
    unifyWith s1 (apply s1 r1) (apply s1 r2)
  (TyProd l1 r1, TyProd l2 r2) -> do
    s1 <- unifyWith s l1 l2
    unifyWith s1 (apply s1 r1) (apply s1 r2)
  _ | t1 == t2 -> return s
  _ -> throwError $ TypeMismatch (apply s t1) (apply s t2)

varBind :: (State TyState :> es, Error TypeError :> es) => Int -> Ty -> Eff es Substitution
varBind u t
  | t == TyVar u = gets tySubst
  | u `Set.member` freeTypeVars t = throwError $ OccursCheck u t
  | otherwise = do
      s <- gets tySubst
      let s' = Map.insert u t s
      modify $ \st -> st {tySubst = s'}
      return s'

-- 型スキームのインスタンス化
instantiate :: (State TyState :> es) => TyScheme -> Eff es Ty
instantiate (Forall vars ty) = do
  nvars <- mapM (const newTyVar) vars
  let s = Map.fromList (zip vars (map (\case TyVar n -> TyVar n; _ -> error "impossible") nvars))
  return $ apply s ty

-- 型の一般化
generalize :: TypeEnv -> Ty -> TyScheme
generalize env ty =
  let vars = Set.toList $ ftv ty `Set.difference` ftv env
   in Forall vars ty

-- 式の型推論
inferExp :: (State TyState :> es, Error TypeError :> es) => TypeEnv -> Exp -> Eff es Ty
inferExp env = \case
  Var x -> case Map.lookup x env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ UnboundVariable x
  Unit -> return TyUnit
  Int _ -> return TyInt
  Bool _ -> return TyBool
  Fun (param, paramTy) body -> do
    let env' = Map.insert param (Forall [] paramTy) env
    bodyTy <- inferExp env' body
    s <- gets tySubst
    return $ TyArr paramTy (apply s bodyTy)
  App e1 e2 -> do
    funTy <- inferExp env e1
    argTy <- inferExp env e2
    s1 <- gets tySubst
    let appliedFunTy = apply s1 funTy
    case appliedFunTy of
      TyArr t1 t2 -> do
        unify t1 argTy
        s2 <- gets tySubst
        return $ apply s2 t2
      _ -> throwError $ ExpectedFunction appliedFunTy
  Let name annotatedTy valueExp bodyExp -> do
    valueTy <- inferExp env valueExp
    s1 <- gets tySubst
    finalTy <- do
      unify (apply s1 valueTy) annotatedTy
      s2 <- gets tySubst
      pure $ apply s2 valueTy
    let currentEnv = apply s1 env
        scheme = generalize currentEnv finalTy
        env' = case name of
          Just n -> Map.insert n scheme env
          Nothing -> env
    inferExp (apply s1 env') bodyExp
  LetRec name (param, paramTy) retTy fun body -> do
    let funTy = TyArr paramTy retTy
        funScheme = Forall [] funTy
        paramScheme = Forall [] paramTy
        env' = Map.insert name funScheme $ Map.insert param paramScheme env
    funBodyTy <- inferExp env' fun
    unify funBodyTy retTy
    s <- gets tySubst
    let finalFunTy = apply s funTy
        finalScheme = generalize env finalFunTy
    inferExp (Map.insert name finalScheme env) body
  If cond then_ else_ -> do
    condTy <- inferExp env cond
    unify condTy TyBool
    thenTy <- inferExp env then_
    elseTy <- inferExp env else_
    unify thenTy elseTy
    s <- gets tySubst
    return $ apply s thenTy
  Pair e1 e2 -> do
    ty1 <- inferExp env e1
    ty2 <- inferExp env e2
    return $ TyProd ty1 ty2
  Fst e -> do
    ty <- inferExp env e
    ty1 <- newTyVar
    ty2 <- newTyVar
    unify ty (TyProd ty1 ty2)
    s <- gets tySubst
    return $ apply s ty1
  Snd e -> do
    ty <- inferExp env e
    ty1 <- newTyVar
    ty2 <- newTyVar
    unify ty (TyProd ty1 ty2)
    s <- gets tySubst
    return $ apply s ty2

-- トップレベルの文の型推論
inferStmt :: (State TyState :> es, Error TypeError :> es) => TypeEnv -> Stmt -> Eff es (TypeEnv, Ty)
inferStmt env = \case
  LetStmt name annotatedTy valueExp -> do
    valueTy <- inferExp env valueExp
    s1 <- gets tySubst
    unify (apply s1 valueTy) annotatedTy
    s2 <- gets tySubst
    let finalTy = apply s2 valueTy
    return
      ( case name of
          Just n -> Map.insert n (generalize env finalTy) env
          Nothing -> env,
        finalTy
      )
  LetRecStmt name (param, paramTy) retTy body -> do
    let funTy = TyArr paramTy retTy
        funScheme = Forall [] funTy
        paramScheme = Forall [] paramTy
        env' = Map.insert name funScheme $ Map.insert param paramScheme env
    bodyTy <- inferExp env' body
    unify bodyTy retTy
    s <- gets tySubst
    let finalTy = apply s funTy
        finalScheme = generalize env finalTy
    return (Map.insert name finalScheme env, finalTy)

-- 型チェックのエントリーポイント
typecheck :: Stmt -> Either TypeError (Ty, Substitution)
typecheck = typecheck' primitivesTypes

typecheck' :: TypeEnv -> Stmt -> Either TypeError (Ty, Substitution)
typecheck' env stmt = runPureEff . runErrorNoCallStack $ do
  (ty, st) <- runState (TyState 0 Map.empty) $ do
    (_, inferredTy) <- inferStmt env stmt
    s <- gets tySubst
    return $ apply s inferredTy
  return (ty, tySubst st)

-- プリミティブ関数の型定義
primitivesTypes :: TypeEnv
primitivesTypes =
  Map.fromList
    [ ("add", Forall [] (TyArr TyInt (TyArr TyInt TyInt))),
      ("sub", Forall [] (TyArr TyInt (TyArr TyInt TyInt))),
      ("mul", Forall [] (TyArr TyInt (TyArr TyInt TyInt))),
      ("div", Forall [] (TyArr TyInt (TyArr TyInt TyInt))),
      ("neg", Forall [] (TyArr TyInt TyInt)),
      ("eq", Forall [0] (TyArr (TyVar 0) (TyArr (TyVar 0) TyBool))),
      ("lt", Forall [] (TyArr TyInt (TyArr TyInt TyBool))),
      ("gt", Forall [] (TyArr TyInt (TyArr TyInt TyBool))),
      ("le", Forall [] (TyArr TyInt (TyArr TyInt TyBool))),
      ("ge", Forall [] (TyArr TyInt (TyArr TyInt TyBool))),
      ("ne", Forall [0] (TyArr (TyVar 0) (TyArr (TyVar 0) TyBool)))
    ]
