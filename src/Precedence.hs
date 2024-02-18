{-# LANGUAGE RankNTypes #-}

module Precedence where

import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Parser

data Precedence = LAssoc Int | RAssoc Int

data Infix s e  = Infix  String Precedence (LedParser s e)
data Prefix s e = Prefix String Precedence (PrefixParser s e)

type PrecedenceParser s e = Precedence -> Parser s e
type NudParser s e = PrecedenceParser s e -> Parser s e
type LedParser s e = Infix s e -> e -> NudParser s e
type PrefixParser s e = Prefix s e -> NudParser s e

buildParser ::
  forall s e .
     Monoid s => 
     [Infix s e]
  -> [Prefix s e]
  -> NudParser s e
  -> Parser s String
  -> Parser s String
  -> Parser s e

buildParser infixes prefixes nud oper strip = parseExpr (RAssoc 0)
  where

    infixMap :: Map.Map String (Infix s e)
    infixMap = intoMap infixes (\op@(Infix key _ _) -> (key, op))
    
    prefixMap :: Map.Map String (Prefix s e)
    prefixMap = intoMap prefixes (\op@(Prefix key _ _) -> (key, op))

    intoMap :: [a] -> (a -> (String, a)) -> Map.Map String a 
    intoMap ops f = Map.fromList (map f ops)

    parseExpr :: Precedence -> Parser s e
    parseExpr assoc = do
        strip
        term <- parsePrefix <|> nud parseExpr
        strip
        parseInfix (precedence assoc) term
      where
        precedence :: Precedence -> Int
        precedence (LAssoc n) = n + 1
        precedence (RAssoc n) = n

    parsePrefix :: Parser s e
    parsePrefix = do
      key <- oper
      case Map.lookup key prefixMap of
        Just op@(Prefix _ _ bind) -> bind op parseExpr 
        Nothing -> err "expected prefix"

    parseInfix :: Int -> e -> Parser s e
    parseInfix precedence lhs = do
        maybeKey <- try (nextOperator precedence)
        case maybeKey of
          Just op@(Infix _ _ led) -> led op lhs parseExpr >>= parseInfix precedence
          Nothing -> pure lhs

    nextOperator :: Int -> Parser s (Infix s e)
    nextOperator n = do
      key <- oper
      case Map.lookup key infixMap of
        Just op@(Infix _ (LAssoc m) _) | m >= n -> pure op
        Just op@(Infix _ (RAssoc m) _) | m >= n -> pure op
        _ -> err "expected infix"