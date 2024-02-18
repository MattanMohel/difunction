import Precedence
import Parser 
import Control.Monad
import Control.Applicative
import Data.Char

data Expr 
    = Appl Expr Expr 
    | Abst Expr Expr
    | Type Expr Expr
    | Arrow Expr Expr 
    | Term String 

instance Show Expr where 
    show (Appl a b)  = "("++show a++" "++show b++")"
    show (Abst a b)  = "(λ"++show a++"."++show b++")"
    show (Type a b)  = "["++show a++": "++show b++"]"
    show (Arrow a b) = "("++show a++" -> "++show b++")"
    show (Term a) = a

var :: Parser Expr
var = Term <$> (some alpha)

nud :: NudParser Expr 
nud pex = var <|> wrap "(" (strip $ pex (RAssoc 0)) ")"

oper :: Parser String 
oper = 
    ((char '\\'   >> pure "Abst")   <|>
     (char ':'    >> pure "Bind")   <|>
     (string "->" >> pure "Arrow")) <|> 
     ((whtspc >> (lift $ noneof ")")) >> pure "Appl")

ops = [
    Infix "Appl" (LAssoc 10) bindAppl,
    Infix "Bind" (LAssoc 20) bindType,
    Infix "Arrow" (RAssoc 30) bindArrow]

pre = [
    Prefix "Abst" (RAssoc 20) bindAbst]

bindType :: LedParser Expr 
bindType (Infix _ precedence _) lhs pex = do 
    rhs <- strip $ pex precedence
    pure $ Type lhs rhs

bindAppl :: LedParser Expr
bindAppl (Infix _ precedence _) lhs pex = do 
    rhs <- pex precedence
    pure $ Appl lhs rhs

bindAbst :: PrefixParser Expr 
bindAbst (Prefix _ precedence _) pex = do
    bind <- strip $ pex precedence
    char '.'
    body <- strip $ pex (RAssoc 0)
    pure $ Abst bind body 

bindArrow :: LedParser Expr
bindArrow (Infix _ precedence _) lhs pex = do 
    rhs <- pex precedence
    pure $ Arrow lhs rhs

lambda = buildParser ops pre nud oper pass

term :: String -> Expr 
term inp = case run (strip lambda <* eof) inp of 
    Err _ -> error $ "invalid lambda term "++inp
    Pass l _ -> l