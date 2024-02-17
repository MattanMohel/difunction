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

nud :: NudParser Expr 
nud pex = var <|> wrap "(" (pex (RAssoc 0)) ")"

oper :: Parser String 
oper = (consumed whtspc >> pure "Appl") <|> (char 'L' >> pure "Abst")

ops = [Infix "Appl" (LAssoc 10) bindAppl]
pre = [Prefix "Abst" (RAssoc 20) bindAbst]

bindAppl :: LedParser Expr
bindAppl (Infix _ precedence _) lhs pex = do 
    rhs <- pex precedence
    pure $ Appl lhs rhs

bindAbst :: PrefixParser Expr 
bindAbst (Prefix _ precedence _) pex = do
    whtspc
    bind <- pex precedence
    whtspc
    char '.'
    whtspc
    body <- pex (RAssoc 0)
    pure $ Abst bind body 

lambda = buildParser ops pre nud oper pass

term :: String -> Expr 
term inp = case run (lambda <* eof) inp of 
    Left _ -> error $ "invalid lmabda term "++inp
    Right (l, _) -> l