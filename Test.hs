module Test where

import Precedence
import Parser 
import Control.Monad
import Control.Applicative

data Expr
  = Binop String Expr Expr
  | Unary String Expr
  | Term Int

instance Show Expr where
  show (Binop key e1 e2) = "("++show e1++key++show e2++")"
  show (Unary key e1) = "("++key++show e1++")"
  show (Term t) = show t

ops = [
  Infix "+" (LAssoc 50) stdInfix,
  Infix "*" (LAssoc 70) stdInfix]

prefixes = [
  Prefix "-" stdPrefix,
  Prefix "+" stdPrefix,
  Prefix "!" stdPrefix]

stdPrefix :: String -> Expr -> Parser Expr
stdPrefix op rhs = pure $ Unary op rhs

stdInfix :: LedParser Expr
stdInfix (Infix op precedence _) lhs pex = pex precedence >>= pure . Binop op lhs

term :: NudParser Expr
term pex = Term <$> number <|> wrap "(" (pex (LAssoc 0)) ")"

oper :: Parser String
oper = (:"") <$> anyof "+*-!"

strip :: Parser String
strip = whtspc

pratt = buildParser ops prefixes term oper strip