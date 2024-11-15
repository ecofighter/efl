{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker
  ( typecheck,
    typecheck',
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

newTyVar :: (State TyState :> es) => Eff es Ty
newTyVar = do
  s <- get
  put s {tyCounter = tyCounter s + 1}
  return $ TyVar (tyCounter s)

freeTypeVars :: Ty -> Set.Set Int
freeTypeVars ty = case ty of
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

unify :: (State TyState :> es, Error TypeError :> es) => Ty -> Ty -> Eff es Substitution
unify t1 t2 = do
  s <- gets tySubst
  s' <- unifyWith s t1 t2
  modify $ \st -> st {tySubst = s'}
  return s'

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
  _ | apply s t1 == apply s t2 -> return s
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

instantiate :: (State TyState :> es) => TyScheme -> Eff es Ty
instantiate (Forall vars ty) = do
  nvars <- mapM (const newTyVar) vars
  let s = Map.fromList (zip vars (map (\case TyVar n -> TyVar n; _ -> error "impossible") nvars))
  return $ apply s ty

generalize :: TypeEnv -> Ty -> TyScheme
generalize env ty =
  let vars = Set.toList $ ftv ty `Set.difference` ftv env
   in Forall vars ty

typecheck' :: TypeEnv -> Exp -> Maybe Ty -> Either TypeError (Ty, Substitution)
typecheck' env expr maybeAnnot = runPureEff . runErrorNoCallStack $ do
  (ty, st) <- runState initialState $ do
    inferredTy <- infer env expr
    case maybeAnnot of
      Just annotTy -> do
        s <- unify inferredTy annotTy
        let resultTy = apply s annotTy
        return resultTy
      Nothing -> return inferredTy
  return (apply (tySubst st) ty, tySubst st)
  where
    initialState = TyState {tyCounter = 0, tySubst = Map.empty}

typecheck :: Exp -> Either TypeError (Ty, Substitution)
typecheck expr = typecheck' primitivesTypes expr Nothing

infer :: (State TyState :> es, Error TypeError :> es) => TypeEnv -> Exp -> Eff es Ty
infer env expr = case expr of
  Var x -> case Map.lookup x env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ UnboundVariable x
  Unit -> return TyUnit
  Int _ -> return TyInt
  Bool _ -> return TyBool
  Fun (param, maybeParamTy) body -> do
    paramTy <- maybe newTyVar return maybeParamTy
    s1 <- gets tySubst
    let paramTy' = apply s1 paramTy
        paramEnv = Map.insert param (Forall [] paramTy') env
    bodyTy <- infer paramEnv body
    s2 <- gets tySubst
    return $ TyArr (apply s2 paramTy') (apply s2 bodyTy)
  App fun arg -> do
    funTy <- infer env fun
    argTy <- infer env arg
    retTy <- newTyVar
    s <- unify funTy (TyArr argTy retTy)
    return $ apply s retTy
  Let maybeName maybeTy value body -> do
    valueTy <- infer env value
    s1 <- gets tySubst
    let valueTy' = apply s1 valueTy
    case maybeTy of
      Just ty -> do
        s2 <- unify valueTy' ty
        let valueTy'' = apply s2 valueTy'
            env' = case maybeName of
              Just name -> Map.insert name (generalize env valueTy'') env
              Nothing -> env
        infer (apply s2 env') body
      Nothing -> do
        let env' = case maybeName of
              Just name -> Map.insert name (generalize env valueTy') env
              Nothing -> env
        infer env' body
  LetRec name (param, maybeParamTy) maybeRetTy fun body -> do
    -- Get parameter and return types
    paramTy <- maybe newTyVar return maybeParamTy
    retTy <- maybe newTyVar return maybeRetTy
    -- Create the expected function type
    let funTy = TyArr paramTy retTy
    -- Add non-polymorphic bindings to environment for recursive function
    let recEnv =
          Map.insert name (Forall [] funTy) $
            Map.insert param (Forall [] paramTy) env
    -- Infer the actual type of function body
    actualBodyTy <- infer recEnv fun
    -- Unify the actual body type with the expected return type and get current substitution
    s1 <- unify actualBodyTy retTy
    let paramTy' = apply s1 paramTy
        retTy' = apply s1 retTy
        funTy' = TyArr paramTy' retTy'
    -- Create new environment with generalized function type, applying the substitution
    let env' = Map.insert name (Forall [] funTy') (apply s1 env)
    -- Infer the type of the body expression
    bodyTy <- infer env' body
    -- Get final substitution and apply it
    s2 <- gets tySubst
    return $ apply s2 bodyTy
  If cond then_ else_ -> do
    condTy <- infer env cond
    s1 <- unify condTy TyBool
    let env' = apply s1 env
    thenTy <- infer env' then_
    elseTy <- infer env' else_
    s2 <- unify thenTy elseTy
    return $ apply s2 thenTy
  Pair e1 e2 -> do
    ty1 <- infer env e1
    ty2 <- infer env e2
    return $ TyProd ty1 ty2
  Fst e -> do
    ty <- infer env e
    ty1 <- newTyVar
    ty2 <- newTyVar
    s <- unify ty (TyProd ty1 ty2)
    return $ apply s ty1
  Snd e -> do
    ty <- infer env e
    ty1 <- newTyVar
    ty2 <- newTyVar
    s <- unify ty (TyProd ty1 ty2)
    return $ apply s ty2

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
