{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Functor
import Data.Set qualified as Set
import Syntax
import Text.Trifecta

reserved :: ByteString -> Bool
reserved s = Set.member s set
  where
    set =
      Set.fromList
        [ "let",
          "rec",
          "in",
          "fun",
          "if",
          "then",
          "else",
          "true",
          "false",
          "match",
          "with",
          "fst",
          "snd"
        ]

parseStmt :: Parser Stmt
parseStmt = parseLetStmt <?> "Statement"

parseLetStmt :: Parser Stmt
parseLetStmt = do
  _ <- symbol "let"
  name <- choice [Just <$> parseIdent, symbolic '_' $> Nothing]
  params <- many param
  ty <- optional tyAnnot
  _ <- symbolic '='
  body <- parseExp
  pure (LetStmt name ty (foldr Fun body params)) <?> "Let Statement"
  where
    param = choice [annotated, fmap (,) parseIdent <*> pure Nothing]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, Just ty)
    tyAnnot = symbolic ':' *> parseTy

parseExp :: Parser Exp
parseExp =
  choice
    [ parseLet,
      parseFun,
      parseIf,
      try parseApp,
      try parseFst,
      try parseSnd,
      parseAtomicExp
    ]
    <?> "Expression"

parseAtomicExp :: Parser Exp
parseAtomicExp =
  choice [parseVar, parseInt, parseBool, try parsePair, parens parseExp]
    <?> "Atomic Expression"

parseVar :: Parser Exp
parseVar = Var <$> parseIdent <?> "Variable"

parseInt :: Parser Exp
parseInt = Int . fromInteger <$> integer <?> "Integer value"

parseBool :: Parser Exp
parseBool = Bool <$> choice [symbol "true" $> True, symbol "false" $> False] <?> "Bool value"

parseFun :: Parser Exp
parseFun = do
  _ <- symbol "fun"
  params <- some param
  _ <- symbol "->"
  body <- parseExp
  pure (foldr Fun body params) <?> "Function"
  where
    param = choice [annotated, fmap (,) parseIdent <*> pure Nothing]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, Just ty)

parseApp :: Parser Exp
parseApp = liftA2 (foldl App) parseAtomicExp (some parseAtomicExp) <?> "Application"

parseLet :: Parser Exp
parseLet = do
  _ <- symbol "let"
  name <- choice [Just <$> parseIdent, symbolic '_' $> Nothing]
  params <- many param
  ty <- optional tyAnnot
  _ <- symbolic '='
  body <- parseExp
  _ <- symbol "in"
  expr <- parseExp
  pure (Let name ty (foldr Fun body params) expr) <?> "Let Expression"
  where
    param = choice [annotated, fmap (,) parseIdent <*> pure Nothing]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, Just ty)
    tyAnnot = symbolic ':' *> parseTy

parseIf :: Parser Exp
parseIf =
  If
    <$ symbol "if"
    <*> parseExp
    <* symbol "then"
    <*> parseExp
    <* symbol "else"
    <*> parseExp <?> "If Expression"

parseIdent :: Parser ByteString
parseIdent = try (token aux) <?> "Identifier"
  where
    aux = do
      res <- sliced $ fmap (:) lower <*> many alphaNum
      if reserved res
        then
          fail "reserved"
        else
          pure res

parsePair :: Parser Exp
parsePair = parens (Pair <$> parseExp <* symbolic ',' <*> parseExp) <?> "Pair Exp"

parseFst :: Parser Exp
parseFst = Fst <$> (symbol "fst" *> parseAtomicExp)

parseSnd :: Parser Exp
parseSnd = Snd <$> (symbol "snd" *> parseAtomicExp)

parseTy :: Parser Ty
parseTy = choice [parseTyArrow, parseAtomicTy] <?> "Type"

parseAtomicTy :: Parser Ty
parseAtomicTy =
  choice [parseTyInt, parseTyBool, try parseTyProd, parens parseTy]
    <?> "Atomic Type"

parseTyInt :: Parser Ty
parseTyInt = symbol "Int" $> TyInt <?> "Type Int"

parseTyBool :: Parser Ty
parseTyBool = symbol "Bool" $> TyBool <?> "Type Bool"

parseTyArrow :: Parser Ty
parseTyArrow = parseAtomicTy `chainr1` arr <?> "Type Arrow"
  where
    arr = TyArr <$ symbol "->"

parseTyProd :: Parser Ty
parseTyProd = parens (TyProd <$> parseTy <* symbolic ',' <*> parseTy) <?> "Product Type"
