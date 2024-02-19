import Precedence
import Parser 
import Control.Monad
import Control.Applicative
import Data.Char

import qualified Data.Map as Map

data Expr 
    = Appl Expr Expr 
    | Abst Var Expr
    | Term Var

    | Type Expr Expr 
    | Arrow Expr Expr 

data Var 
    = Free String 
    | Bind String Int -- De Brujin index

instance Eq Var where 
    a == b = case (a, b) of 
        (Free m, Free n) -> m == n
        (Bind _ m, Bind _ n) -> m == n
        _ -> False

instance Show Var where 
    show (Free name)     = name
    show (Bind name idx) = name

instance Show Expr where 
    show (Appl a b)  = "("++show a++" "++show b++")"
    show (Abst a b)  = "(λ"++show a++"."++show b++")"
    show (Term a) = show a
    show (Type a b) = "["++show a++": "++show b++"]"
    show (Arrow a b) = "("++show a++" -> "++show b++")"

instance Eq Expr where 
    a == b = case (a, b) of 
        (Abst m n, Abst p q) -> m == p && n == q
        (Appl m n, Appl p q) -> n == p && n == q
        (Term m, Term n) -> m == n
        _ -> False

subterms :: Expr -> [Expr]
subterms expr = impl expr []
    where 
        impl :: Expr -> [Expr] -> [Expr]
        impl (Abst a b) acc = Abst a b : Term a : subterms b ++ acc
        impl (Appl a b) acc = Appl a b : subterms a ++ subterms b ++ acc
        impl (Term a) acc = Term a : acc

isSubterm :: Expr -> Expr -> Bool
isSubterm sub expr = expr /= sub && sub `elem` subterms expr

freeVars :: Expr -> [Expr] 
freeVars expr = impl expr []
    where 
        impl :: Expr -> [Expr] -> [Expr]
        impl term@(Term (Free _)) acc = term : acc
        impl (Appl a b) acc = freeVars a ++ freeVars b ++ acc
        impl (Abst a b) acc = freeVars b ++ acc
        impl _ acc = acc

isFreeVar :: Expr -> Expr -> Bool 
isFreeVar var expr = var `elem` freeVars expr

subst :: Expr -> Var -> Expr -> Expr 
subst (Appl a b) term sub = Appl (subst a term sub) (subst b term sub) 
subst (Term a) term sub = if a == term then sub else Term a 
subst expr _ _ = expr

betaReduce :: Expr -> Expr 
betaReduce (Appl (Abst a b) c) = betaReduce $ subst b a c
betaReduce expr = expr

-- Parsing Lambda Terms 
-- 
-- The parser's role is to take the input stream and 
-- structure it into a valid evaluation tree 
-- 
-- Once the parser returns the Expression tree, it is 
-- already validated to be a correct program 

data Context = Context (Map.Map String Int) Int

bind :: String -> Context -> Context 
bind var (Context gamma n) = Context (Map.insert var n gamma) (n + 1)

find :: String -> Context -> Parser s Var 
find var (Context map _) = case Map.lookup var map of 
    Just n -> pure $ Bind var n
    Nothing -> empty 
 
nud :: NudParser Context Expr 
nud pex = var <|> wrap "(" (strip $ pex (RAssoc 0)) ")"

oper :: Parser Context String 
oper = 
     (char '\\' >> pure "Abst") <|> 
     (whtspc >> (lift $ noneof ")") >> pure "Appl")

ops = [
    Infix "Appl" (LAssoc 10) bindAppl]

pre = [
    Prefix "Abst" (RAssoc 20) bindAbst]

var :: Parser Context Expr
var = do 
    (Context map _) <- (|-)
    name  <- some alpha 
    case Map.lookup name map of 
        Just n  -> pure (Term $ Bind name n)
        Nothing -> pure (Term $ Free name)

bindAppl :: LedParser Context Expr
bindAppl (Infix _ precedence _) lhs pex = do 
    rhs <- pex precedence
    pure $ Appl lhs rhs

bindAbst :: PrefixParser Context Expr 
bindAbst (Prefix _ precedence _) pex = do
    gamma <- (|-)
    name  <- strip $ some alpha
    (|=) $ bind name 
    bind <- (|-) >>= find name
    char '.'
    body <- strip $ pex (RAssoc 0)
    (|=) $ \_ -> gamma
    pure $ Abst bind body 

lambda = buildParser ops pre nud oper pass

term :: String -> Expr 
term stream = case parse (strip lambda <* eof) (stream, (0, 0, 0), Context Map.empty 0) of 
    Err e -> error $ show e
    Pass l _ -> l






bindType :: LedParser Context Expr
bindType (Infix _ precedence _) lhs pex = do 
    rhs <- strip $ pex precedence
    pure $ Type lhs rhs 

bindArrow :: LedParser Context Expr
bindArrow (Infix _ precedence _) lhs pex = do 
    rhs <- strip $ pex precedence
    pure $ Arrow lhs rhs 
