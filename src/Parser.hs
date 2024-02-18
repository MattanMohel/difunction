{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser where

import Control.Applicative
import Data.Char

type State s = ((Int, Int, Int), s)
type Inp s = (String, State s)

data Out s a 
  = Err  [(ParseErr, State s)] 
  | Pass a (Inp s)

index :: Inp s -> Int 
index (_, ((_, _, e), _)) = e

state :: Inp s -> s 
state (_, (_, e)) = e

advance :: Char -> State s -> State s
advance x ((row, col, idx), state) = case x of
  '\n' -> ((row + 1, col, idx + 1), state)
  _    -> ((row, col + 1, idx + 1), state)

instance (Show s, Show a) => Show (Out s a) where 
  show (Err err)     = show err
  show (Pass out s)  = "["++(show out)++", "++(show (state s))++"]"

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

newtype Parser s a = Parser (Inp s -> Out s a)

instance Functor (Parser s) where
  fmap fun p = Parser $ \inp -> case parse p inp of
    Pass out inp -> Pass (fun out) inp
    Err err -> Err err

instance Applicative (Parser s) where
  pure a = Parser $ \inp -> (Pass a inp)
  pfun <*> p = Parser $ \inp -> case parse pfun inp of
    Pass fun inp -> parse (fmap fun p) inp
    Err err -> Err err

instance Monad (Parser s) where
  p >>= fun = Parser $ \inp -> case parse p inp of
    Pass out inp -> parse (fun out) inp
    Err err -> Err err

instance Monoid s => Alternative (Parser s) where
  empty = Parser $ \_ -> Err [(Empty, ((0, 0, 0), mempty))]
  p <|> q = Parser $ \inp -> case parse p inp of
    Pass out inp -> Pass out inp
    Err errp -> case parse q inp of
      Pass out inp -> Pass out inp
      Err errq -> Err $ errp ++ errq

parse :: Parser s a -> Inp s -> Out s a
parse (Parser p) = p

run :: Monoid s => Parser s a -> String -> Out s a
run (Parser parser) stream = parser (stream, ((0, 0, 0), mempty))

entry :: Parser () a -> String -> Out () a 
entry p = run p

item :: Parser s Char
item = Parser $ \(stream, state) -> case stream of
  (x:xs) -> Pass x (xs, advance x state)
  "" -> Err [(EndOfInput, state)]

expect :: Parser s a -> String -> Parser s a
expect p mes = Parser $ \inp@(_, state) -> case parse p inp of
  Pass out inp -> Pass out inp
  Err _ -> Err [(Expect mes, state)]

-- consumes no input on success
try :: Parser s a -> Parser s (Maybe a)
try p = Parser $ \inp -> case parse p inp of
  Err _ -> Pass Nothing inp
  Pass out inp -> Pass (Just out) inp

-- run parser without consuming input
lift :: Parser s a -> Parser s a
lift p = Parser $ \inp -> case parse p inp of 
  Pass out _ -> Pass out inp
  Err err -> Err err

consumed :: Parser s a -> Parser s a
consumed p = Parser $ \inp@(_, state) -> case parse p inp of 
  pass@(Pass _ inp') -> if (index inp') == (index inp)
    then Err [(Custom "didn't consume", state)]  
    else pass
  Err err -> Err err

pass :: Monoid a => Parser s a
pass = Parser $ \inp -> Pass mempty inp

halt :: Parser s a
halt = Parser $ \(_, state) -> Err [(Empty, state)]

err :: String -> Parser s a
err mes = Parser $ \(_, state) -> Err [(Custom mes, state)]

sat :: (Char -> Bool) -> Parser s Char
sat fun = item >>= \x -> if fun x then pure x else halt 

char :: Char -> Parser s Char
char c = expect (sat (==c)) [c]

range :: Char -> Char -> Parser s Char
range a b = expect (sat $ \x -> x >= a && x <= b) (a:'-':b:"")

list :: Parser s a -> Parser s [a]
list p = (:[]) <$> p

string :: String -> Parser s String
string = foldr (\x -> (<*>) ((:) <$> char x)) (pure [])

eof :: Parser s ()
eof = Parser $ \inp@(_, state) -> case parse item inp of
  Err _ -> Pass () inp
  Pass _ _ -> Err [(Expect "end of input", state)]

notEof :: Parser s Char
notEof = lift item 

alpha :: Parser s Char
alpha = expect (sat $ \x -> isAlpha x) "alpha"

numeric :: Parser s Char
numeric = expect (sat $ \x -> isDigit x) "numeric"

alphanum :: Parser s Char
alphanum = expect (sat $ \x -> isAlpha x || isDigit x) "alphanum"

anyof :: String -> Parser s Char
anyof s = expect (sat $ \x -> x `elem` s) ("any of "++s)

noneof :: String -> Parser s Char
noneof s = expect (sat $ \x -> x `notElem` s) ("noneof of "++s)

whtspc :: Monoid s => Parser s String
whtspc = expect (many $ anyof "\n\t ") "whitespace"

between :: Parser s a -> Parser s b -> Parser s c -> Parser s b
between lhs p rhs = lhs *> p <* rhs

wrap :: String -> Parser s a -> String -> Parser s a
wrap lhs p rhs = between (string lhs) p (string rhs)

strip :: Monoid s => Parser s a -> Parser s a
strip p = whtspc *> p <* whtspc

digit :: Parser s Int
digit = read . (:[]) <$> numeric

number :: Monoid s => Parser s Int
number = read <$> some numeric

context :: (s -> a -> s) -> a -> Parser s a
context fun x = Parser $ \(stream, (pos, state)) -> Pass x (stream, (pos, fun state x))