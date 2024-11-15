{-# LANGUAGE StrictData #-}
module Syntax where

import Data.ByteString.Char8 (ByteString)

data Ty
  = TyUnit
  | TyInt
  | TyBool
  | TyArr Ty Ty
  | TyProd Ty Ty
  | TyVar Int
  deriving (Eq, Show)

data Exp
  = Var ByteString
  | Unit
  | Int Int
  | Bool Bool
  | Fun (ByteString, Ty) Exp
  | App Exp Exp
  | Let (Maybe ByteString) Ty Exp Exp
  | LetRec ByteString (ByteString, Ty) Ty Exp Exp
  | If Exp Exp Exp
  | Pair Exp Exp
  | Fst Exp
  | Snd Exp
  deriving (Eq, Show)

data Stmt
  = LetStmt (Maybe ByteString) Ty Exp
  | LetRecStmt ByteString (ByteString, Ty) Ty Exp
