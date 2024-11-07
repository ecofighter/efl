{-# LANGUAGE StrictData #-}
module Syntax where

import Data.ByteString.Char8 (ByteString)

data Ty
  = TyInt
  | TyBool
  | TyArr Ty Ty
  | TyVar Int
  deriving (Eq, Show)

data Stage = Time Int | Infinite
  deriving (Eq, Show)

data StagedTy = StagedTy Stage Ty
  deriving (Eq, Show)

data Exp
  = Var ByteString
  | Int Int
  | Bool Bool
  | Fun (ByteString, Maybe Ty) Exp
  | App Exp Exp
  | Let ByteString (Maybe Ty) Exp Exp
  | If Exp Exp Exp
  deriving (Eq, Show)

data Stmt
  = LetStmt ByteString (Maybe Ty) Exp
  | ExpStmt Exp
