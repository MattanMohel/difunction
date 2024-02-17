module Precedence where

import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Parser

data Precedence = LAssoc Int | RAssoc Int

data Infix e  = Infix String Precedence (LedParser e)
data Prefix e = Prefix String (String -> e -> Parser e)

type OperatorMap o = Map.Map String o
type PrecedenceParser e = Precedence -> Parser e
type NudParser e = PrecedenceParser e -> Parser e
type LedParser e = Infix e -> e -> PrecedenceParser e -> Parser e
type PrefixParser e = String -> PrecedenceParser e -> Parser e

buildParser ::
  forall e .
     [Infix e]
  -> [Prefix e]
  -> NudParser e
  -> Parser String
  -> Parser String
  -> Parser e

buildParser infixes prefixes nud oper strip = parseExpr (RAssoc 0)
  where

    infixMap :: OperatorMap (Infix e)
    infixMap = intoMap infixes (\op@(Infix key _ _) -> (key, op))
    
    prefixMap :: OperatorMap (Prefix e)
    prefixMap = intoMap prefixes (\op@(Prefix key _) -> (key, op))

    intoMap :: [a] -> (a -> (String, a)) -> OperatorMap a 
    intoMap ops f = Map.fromList (map f ops)

    parseExpr :: Precedence -> Parser e
    parseExpr assoc = do
        strip
        term <- parsePrefix <|> nud parseExpr
        strip
        parseInfix (precedence assoc) term
      where
        precedence :: Precedence -> Int
        precedence (LAssoc n) = n + 1
        precedence (RAssoc n) = n

    parsePrefix :: Parser e
    parsePrefix = do
      key <- oper
      case Map.lookup key prefixMap of
        Just (Prefix _ bind) -> nud parseExpr >>= bind key
        Nothing -> empty

    parseInfix :: Int -> e -> Parser e
    parseInfix precedence lhs = do
        maybeKey <- try (nextOperator precedence)
        case maybeKey of
          Just op@(Infix _ _ led) -> led op lhs parseExpr >>= parseInfix precedence
          Nothing -> pure lhs

    nextOperator :: Int -> Parser (Infix e)
    nextOperator n = do
      key <- oper
      case Map.lookup key infixMap of
        Just op@(Infix _ (LAssoc m) _) | m >= n -> pure op
        Just op@(Infix _ (RAssoc m) _) | m >= n -> pure op
        _ -> empty