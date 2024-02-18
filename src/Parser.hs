{-# LANGUAGE TypeSynonymInstances #-}

module Parser where

import Control.Applicative
import Data.Char

type State = (Int, Int, Int)
type Inp   = (String, State)

data Out a 
  = Err  [(ParseErr, State)] 
  | Pass a Inp

index :: Inp -> Int 
index (_, (_, _, i)) = i

instance Show a => Show (Out a) where 
  show (Err err)     = show err
  show (Pass out _)  = show out

instance Show ParseErr where 
  show (Expect mes) = "expected \""++mes++"\""
  show (Custom mes) = mes
  show EndOfInput = "end of input"
  show Empty = "empty"
  
data ParseErr
  = Expect String
  | Custom String
  | EndOfInput
  | Empty

newtype Parser a = Parser (Inp -> Out a)

instance Functor Parser where
  fmap fun p = Parser $ \inp -> case parse p inp of
    Pass out inp -> Pass (fun out) inp
    Err err -> Err err

instance Applicative Parser where
  pure a = Parser $ \inp -> (Pass a inp)
  pfun <*> p = Parser $ \inp -> case parse pfun inp of
    Pass fun inp -> parse (fmap fun p) inp
    Err err -> Err err

instance Monad Parser where
  p >>= fun = Parser $ \inp -> case parse p inp of
    Pass out inp -> parse (fun out) inp
    Err err -> Err err

instance Alternative Parser where
  empty = Parser $ \_ -> Err [(Empty, (0, 0, 0))]
  p <|> q = Parser $ \inp -> case parse p inp of
    Pass out inp -> Pass out inp
    Err errp -> case parse q inp of
      Pass out inp -> Pass out inp
      Err errq -> Err $ errp ++ errq

advance :: Char -> State -> State
advance x (row, col, idx) = case x of
  '\n' -> (row + 1, col, idx + 1)
  _    -> (row, col + 1, idx + 1)

parse :: Parser a -> Inp -> Out a
parse (Parser p) = p

run :: Parser a -> String -> Out a
run (Parser parser) stream = parser (stream, (0, 0, 0))

err :: Parser a -> String -> Parser a
err p mes = Parser $ \inp@(_, state) -> case parse p inp of
  Pass out inp -> Pass out inp
  Err _ -> Err [(Custom mes, state)]

pass :: Monoid a => Parser a
pass = Parser $ \inp -> Pass mempty inp

-- run parser without consuming input
lift :: Parser a -> Parser a
lift p = Parser $ \inp -> case parse p inp of 
  Pass out _ -> Pass out inp
  Err err -> Err err

halt :: Parser a
halt = Parser $ \(_, state) -> Err [(Empty, state)]

consumed :: Parser a -> Parser a
consumed p = Parser $ \inp@(_, state) -> case parse p inp of 
  pass@(Pass _ inp') -> if (index inp') == (index inp)
    then Err [(Custom "didn't consume", state)]  
    else pass
  Err err -> Err err

expect :: Parser a -> String -> Parser a
expect p mes = Parser $ \inp@(_, state) -> case parse p inp of
  Pass out inp -> Pass out inp
  Err _ -> Err [(Expect mes, state)]

item :: Parser Char
item = Parser $ \(stream, state) -> case stream of
  (x:xs) -> Pass x (xs, advance x state)
  [] -> Err [(EndOfInput, state)]

sat :: (Char -> Bool) -> Parser Char
sat fun = item >>= \x -> if fun x then pure x else empty

char :: Char -> Parser Char
char c = expect (sat (==c)) [c]

range :: Char -> Char -> Parser Char
range a b = expect (sat $ \x -> x >= a && x <= b) (a:'-':b:"")

list :: Parser a -> Parser [a]
list p = (:[]) <$> p

string :: String -> Parser String
string = foldr (\x -> (<*>) ((:) <$> char x)) (pure [])

eof :: Parser ()
eof = Parser $ \inp@(_, state) -> case parse item inp of
  Err _ -> Pass () inp
  Pass _ _ -> Err [(Expect "end of input", state)]

notEof :: Parser Char
notEof = lift item 

alpha :: Parser Char
alpha = expect (sat $ \x -> isAlpha x) "alpha"

numeric :: Parser Char
numeric = expect (sat $ \x -> isDigit x) "numeric"

alphanum :: Parser Char
alphanum = expect (sat $ \x -> isAlpha x || isDigit x) "alphanum"

anyof :: String -> Parser Char
anyof s = expect (sat $ \x -> x `elem` s) ("any of "++s)

noneof :: String -> Parser Char
noneof s = expect (sat $ \x -> x `notElem` s) ("noneof of "++s)

whtspc :: Parser String
whtspc = expect (many $ anyof "\n\t ") "whitespace"

between :: Parser a -> Parser b -> Parser c -> Parser b
between lhs p rhs = lhs *> p <* rhs

wrap :: String -> Parser a -> String -> Parser a
wrap lhs p rhs = between (string lhs) p (string rhs)

strip :: Parser a -> Parser a
strip p = whtspc *> p <* whtspc

digit :: Parser Int
digit = read . (:[]) <$> numeric

number :: Parser Int
number = read <$> some numeric

-- consumes no input on success
try :: Parser a -> Parser (Maybe a)
try p = Parser $ \inp -> case parse p inp of
  Err _ -> Pass Nothing inp
  Pass out inp -> Pass (Just out) inp
