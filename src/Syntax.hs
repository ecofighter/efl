{-# LANGUAGE StrictData #-}
module Syntax where

import Data.ByteString.Char8 (ByteString)

data Ty
  = TyInt
  | TyBool
  | TyArr Ty Ty
  | TyProd Ty Ty
  | TyVar Int
  deriving (Eq, Show)

data Pattern
  = PVar ByteString
  | PPair Pattern Pattern
  deriving (Eq, Show)

data Exp
  = Var ByteString
  | Int Int
  | Bool Bool
  | Fun (ByteString, Maybe Ty) Exp
  | App Exp Exp
  | Let ByteString (Maybe Ty) Exp Exp
  | If Exp Exp Exp
  | Pair Exp Exp
  | Fst Exp
  | Snd Exp
  deriving (Eq, Show)

data Stmt
  = LetStmt ByteString (Maybe Ty) Exp
  | ExpStmt Exp
