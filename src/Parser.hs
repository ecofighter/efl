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
parseStmt = choice [try parseLetRecStmt, try parseLetStmt] <* eof <?> "Statement"

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

parseLetRecStmt :: Parser Stmt
parseLetRecStmt = do
  _ <- symbol "let"
  _ <- symbol "rec"
  name <- parseIdent
  params <- some param
  ty <- optional tyAnnot
  _ <- symbolic '='
  body <- parseExp
  pure (LetRecStmt name (head params) ty (foldr Fun body (tail params))) <?> "Let Statement"
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
    [ try parseLetRec,
      try parseLet,
      try parseFun,
      try parseIf,
      try parseApp,
      try parseFst,
      try parseSnd,
      parseAtomicExp
    ]
    <?> "Expression"

parseAtomicExp :: Parser Exp
parseAtomicExp =
  choice [parseVar, parseUnit, parseInt, parseBool, try parsePair, parens parseExp]
    <?> "Atomic Expression"

parseVar :: Parser Exp
parseVar = Var <$> parseIdent <?> "Variable"

parseUnit :: Parser Exp
parseUnit = symbol "()" $> Unit <?> "Unit value"

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
  returnTy <- optional tyAnnot
  _ <- symbolic '='
  body <- parseExp
  _ <- symbol "in"
  expr <- parseExp
  let wholeTy = case returnTy of
        Nothing -> Nothing
        Just retTy -> Just $ foldr TyArr retTy [t | (_, Just t) <- params]
  pure (Let name wholeTy (foldr Fun body params) expr)
  where
    param = choice [annotated, fmap (,) parseIdent <*> pure Nothing]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, Just ty)
    tyAnnot = symbolic ':' *> parseTy

parseLetRec :: Parser Exp
parseLetRec = do
  _ <- symbol "let"
  _ <- symbol "rec"
  name <- parseIdent
  params <- some param
  ty <- optional tyAnnot
  _ <- symbolic '='
  body <- parseExp
  _ <- symbol "in"
  expr <- parseExp
  pure (LetRec name (head params) ty (foldr Fun body (tail params)) expr) <?> "Let Expression"
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

parseTyUnit :: Parser Ty
parseTyUnit = symbol "Unit" $> TyUnit <?> "Type Unit"

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
