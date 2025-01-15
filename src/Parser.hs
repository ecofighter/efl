{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parser (parseStmtFromBS, parseExpFromBS) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import Data.Functor
import Data.Set qualified as Set
import Syntax
import Text.Trifecta

parseStmtFromBS :: ByteString -> Either String Stmt
parseStmtFromBS input = case parseByteString (evalStateT (parseStmt <* eof) (ParserState 10000)) mempty input of
  Success stmt -> Right stmt
  Failure e -> Left (show e)

parseExpFromBS :: ByteString -> Either String Exp
parseExpFromBS input = case parseByteString (evalStateT (parseExp <* eof) (ParserState 10000)) mempty input of
  Success expr -> Right expr
  Failure e -> Left (show e)

newtype ParserState = ParserState {counter :: Int}

type EflParser = StateT ParserState Parser

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

parseStmt :: EflParser Stmt
parseStmt = choice [try parseLetRecStmt, try parseLetStmt] <?> "Statement"

parseLetStmt :: EflParser Stmt
parseLetStmt = do
  _ <- symbol "let"
  name <- choice [Just <$> parseIdent, symbolic '_' $> Nothing]
  params <- many param
  ty <- choice [try tyAnnot, freshTyVar]
  _ <- symbolic '='
  body <- parseExp
  let wholeTy = foldr TyArr ty [t | (_, t) <- params]
  pure (LetStmt name wholeTy (foldr Fun body params)) <?> "Let Statement"
  where
    param = choice [annotated, fmap (,) parseIdent <*> freshTyVar]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, ty)
    tyAnnot = symbolic ':' *> parseTy

parseLetRecStmt :: EflParser Stmt
parseLetRecStmt = do
  _ <- symbol "let"
  _ <- symbol "rec"
  name <- parseIdent
  paramsHead <- param
  paramsTail <- many param
  ty <- choice [try tyAnnot, freshTyVar]
  _ <- symbolic '='
  body <- parseExp
  let wholeTy = foldr TyArr ty [t | (_, t) <- (paramsHead : paramsTail)]
  pure (LetRecStmt name paramsHead wholeTy (foldr Fun body paramsTail)) <?> "LetRec Statement"
  where
    param = choice [annotated, fmap (,) parseIdent <*> freshTyVar]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, ty)
    tyAnnot = symbolic ':' *> parseTy

parseExp :: EflParser Exp
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

parseAtomicExp :: EflParser Exp
parseAtomicExp =
  choice [parseVar, parseUnit, parseInt, parseBool, try parsePair, parens parseExp]
    <?> "Atomic Expression"

parseVar :: EflParser Exp
parseVar = Var <$> parseIdent <?> "Variable"

parseUnit :: EflParser Exp
parseUnit = symbol "()" $> Unit <?> "Unit value"

parseInt :: EflParser Exp
parseInt = Int . fromInteger <$> integer <?> "Integer value"

parseBool :: EflParser Exp
parseBool = Bool <$> choice [symbol "true" $> True, symbol "false" $> False] <?> "Bool value"

parseFun :: EflParser Exp
parseFun = do
  _ <- symbol "fun"
  params <- some param
  _ <- symbol "->"
  body <- parseExp
  pure (foldr Fun body params) <?> "Function"
  where
    param = choice [annotated, fmap (,) parseIdent <*> freshTyVar]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, ty)

parseApp :: EflParser Exp
parseApp = liftA2 (foldl App) parseAtomicExp (some parseAtomicExp) <?> "Application"

parseLet :: EflParser Exp
parseLet = do
  _ <- symbol "let"
  name <- choice [Just <$> parseIdent, symbolic '_' $> Nothing]
  params <- many param
  ty <- choice [try tyAnnot, freshTyVar]
  _ <- symbolic '='
  body <- parseExp
  _ <- symbol "in"
  expr <- parseExp
  let wholeTy = foldr TyArr ty [t | (_, t) <- params]
  pure (Let name wholeTy (foldr Fun body params) expr)
  where
    param = choice [annotated, fmap (,) parseIdent <*> freshTyVar]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, ty)
    tyAnnot = symbolic ':' *> parseTy

parseLetRec :: EflParser Exp
parseLetRec = do
  _ <- symbol "let"
  _ <- symbol "rec"
  name <- parseIdent
  paramsHead <- param
  paramsTail <- many param
  ty <- choice [try tyAnnot, freshTyVar]
  _ <- symbolic '='
  body <- parseExp
  _ <- symbol "in"
  expr <- parseExp
  let wholeTy = foldr TyArr ty [t | (_, t) <- (paramsHead : paramsTail)]
  pure (LetRec name paramsHead wholeTy (foldr Fun body paramsTail) expr) <?> "Let Expression"
  where
    param = choice [annotated, fmap (,) parseIdent <*> freshTyVar]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, ty)
    tyAnnot = symbolic ':' *> parseTy

parseIf :: EflParser Exp
parseIf =
  If
    <$ symbol "if"
    <*> parseExp
    <* symbol "then"
    <*> parseExp
    <* symbol "else"
    <*> parseExp <?> "If Expression"

parseIdent :: EflParser ByteString
parseIdent = try (token aux) <?> "Identifier"
  where
    aux = do
      res <- sliced $ fmap (:) lower <*> many alphaNum
      if reserved res
        then
          fail "reserved"
        else
          pure res

parsePair :: EflParser Exp
parsePair = parens (Pair <$> parseExp <* symbolic ',' <*> parseExp) <?> "Pair Exp"

parseFst :: EflParser Exp
parseFst = Fst <$> (symbol "fst" *> parseAtomicExp)

parseSnd :: EflParser Exp
parseSnd = Snd <$> (symbol "snd" *> parseAtomicExp)

freshTyVar :: EflParser Ty
freshTyVar = do
  s <- get
  put $ ParserState (counter s + 1)
  pure $ TyVar (counter s)

parseTy :: EflParser Ty
parseTy = choice [parseTyArrow, parseAtomicTy] <?> "Type"

parseAtomicTy :: EflParser Ty
parseAtomicTy =
  choice [parseTyUnit, parseTyInt, parseTyBool, try parseTyProd, parens parseTy]
    <?> "Atomic Type"

parseTyUnit :: EflParser Ty
parseTyUnit = symbol "Unit" $> TyUnit <?> "Type Unit"

parseTyInt :: EflParser Ty
parseTyInt = symbol "Int" $> TyInt <?> "Type Int"

parseTyBool :: EflParser Ty
parseTyBool = symbol "Bool" $> TyBool <?> "Type Bool"

parseTyArrow :: EflParser Ty
parseTyArrow = parseAtomicTy `chainr1` arr <?> "Type Arrow"
  where
    arr = TyArr <$ symbol "->"

parseTyProd :: EflParser Ty
parseTyProd = parens (TyProd <$> parseTy <* symbolic ',' <*> parseTy) <?> "Product Type"
