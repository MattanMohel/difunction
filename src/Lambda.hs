import Precedence
import Parser 
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List

import qualified Data.Map as Map

data Expr 
    = Appl Expr Expr 
    | Abst Expr Expr
    | Type Expr Expr
    | Arrow Expr Expr 
    | Term String 

instance Eq Expr where 
    a == b = case (a, b) of 
        (Abst m n, Abst p q) -> m == p && n == q
        (Appl m n, Appl p q) -> n == p && n == q
        (Term m, Term n)     -> m == n
        _ -> False

subterms :: Expr -> [Expr]
subterms expr = nub $ subtermRec expr []
    where
        subtermRec :: Expr -> [Expr] -> [Expr]
        subtermRec expr acc = case expr of
            abst@(Abst a b) -> abst : a : (subtermRec b acc)
            appl@(Appl a b) -> appl : (subtermRec a acc)++(subtermRec b acc)
            term@(Term _)   -> term : acc

isSubterm :: Expr -> Expr -> Bool 
isSubterm expr sub = sub /= expr && sub `elem` (subterms expr)

-- | Context stores the variable types of the program 
newtype Context = Context (Map.Map String Expr)

instance Semigroup Context where 
    (Context a) <> (Context b) = Context (a <> b)

instance Monoid Context where 
    mempty = Context Map.empty

instance Show Expr where 
    show (Appl a b)  = show a++" "++show b
    show (Abst a b)  = "(λ"++show a++"."++show b++")"
    show (Type a b)  = "["++show a++": "++show b++"]"
    show (Arrow a b) = show a++" -> "++show b
    show (Term a) = a

var :: Parser Context Expr
var = Term <$> (some alpha)

nud :: NudParser Context Expr 
nud pex = var <|> wrap "(" (strip $ pex (RAssoc 0)) ")"

oper :: Parser Context String 
oper = 
     (char '\\' >> pure "Abst")              <|> 
     (whtspc >> char ':' >> pure "Bind")     <|>
     (whtspc >> string "->" >> pure "Arrow") <|> 
     (whtspc >> (lift $ noneof ")") >> pure "Appl")

ops = [
    Infix "Appl"  (LAssoc 10) bindAppl,
    Infix "Bind"  (LAssoc 20) bindType,
    Infix "Arrow" (RAssoc 30) bindArrow]

pre = [
    Prefix "Abst" (RAssoc 20) bindAbst]

bindType :: LedParser Context Expr 
bindType (Infix _ precedence _) lhs pex = do 
    rhs <- strip $ pex precedence
    pure $ Type lhs rhs

bindAppl :: LedParser Context Expr
bindAppl (Infix _ precedence _) lhs pex = do 
    rhs <- pex precedence
    pure $ Appl lhs rhs

bindAbst :: PrefixParser Context Expr 
bindAbst (Prefix _ precedence _) pex = do
    bind <- strip $ pex precedence
    char '.'
    body <- strip $ pex (RAssoc 0)
    pure $ Abst bind body 

bindArrow :: LedParser Context Expr
bindArrow (Infix _ precedence _) lhs pex = do 
    rhs <- strip $ pex precedence
    pure $ Arrow lhs rhs

lambda = buildParser ops pre nud oper pass

term :: String -> Expr 
term inp = case run (strip lambda <* eof) inp of 
    Err _ -> error $ "invalid lambda term "++inp
    Pass l _ -> l