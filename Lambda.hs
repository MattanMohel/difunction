import Precedence
import Parser 
import Control.Monad
import Control.Applicative
import Data.Char

data Expr 
    = Appl Expr Expr 
    | Abst Expr Expr
    | Term String 

instance Show Expr where 
    show (Appl a b) = "("++show a++" "++show b++")"
    show (Abst a b) = "(L"++show a++"."++show b++")"
    show (Term a) = a

var :: Parser Expr
var = Term <$> (some $ sat (\x -> isAlpha x && isLower x))

term :: NudParser Expr 
term p = var

strip :: Parser String 
strip = pass

oper :: Parser String 
oper = (consumed whtspc >> pure "Appl") <|> (char 'L' >> pure "Abst")

ops = [Infix "Appl" (LAssoc 10) bindAppl]

pre = [Prefix "Abst" bindAbst]

bindAppl :: LedParser Expr
bindAppl (Infix _ precedence _) lhs pex = do 
    rhs <- pex precedence
    pure $ Appl lhs rhs

-- TODO: create prefixBinder that takes pex as arg
bindAbst :: String -> Expr -> Parser Expr 
bindAbst _ rhs = do 
    char '.'
    rest <- lambda
    pure $ Abst rhs rest

    
lambda = buildParser ops pre term oper strip

-- \\ X . A B C --> Space is an operator