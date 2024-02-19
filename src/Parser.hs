{-# LANGUAGE RankNTypes #-}

module Parser where

import Control.Applicative
import Data.Char  

type Pos = (Int, Int, Int)
type Inp s = (String, Pos, s)

data Out s a 
  = Err  [(ParseErr, Pos)] 
  | Pass a (Inp s)

index :: Pos -> Int 
index (_, _, e) = e

state :: Inp s -> s 
state (_, _, e) = e

advance :: Char -> Pos -> Pos
advance x (row, col, idx) = case x of
  '\n' -> (row + 1, col, idx + 1)
  _    -> (row, col + 1, idx + 1)

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

instance Alternative (Parser s) where
  empty = Parser $ \_ -> Err [(Empty, (0, 0, 0))]
  p <|> q = Parser $ \inp -> case parse p inp of
    Pass out inp -> Pass out inp
    Err errp -> case parse q inp of
      Pass out inp -> Pass out inp
      Err errq -> Err $ errp ++ errq

parse :: Parser s a -> Inp s -> Out s a
parse (Parser p) = p

run :: Monoid s => Parser s a -> String -> Out s a
run (Parser parser) stream = parser (stream, (0, 0, 0), mempty)

entry :: Parser () a -> String -> Out () a 
entry p = run p

item :: Parser s Char
item = Parser $ \(stream, pos, state) -> case stream of
  (x:xs) -> Pass x (xs, advance x pos, state)
  "" -> Err [(EndOfInput, pos)]

expect :: Parser s a -> String -> Parser s a
expect p mes = Parser $ \inp@(_, pos, _) -> case parse p inp of
  Pass out inp -> Pass out inp
  Err _ -> Err [(Expect mes, pos)]

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
consumed p = Parser $ \inp@(_, pos, _) -> case parse p inp of 
  pass@(Pass _ (_, pos', _)) -> if (index pos') == (index pos)
    then Err [(Custom "didn't consume", pos)]  
    else pass
  Err err -> Err err

pass :: Monoid a => Parser s a
pass = Parser $ \inp -> Pass mempty inp

halt :: Parser s a
halt = Parser $ \(_, pos, _) -> Err [(Empty, pos)]

err :: String -> Parser s a
err mes = Parser $ \(_, pos, _) -> Err [(Custom mes, pos)]

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
eof = Parser $ \inp@(_, pos, _) -> case parse item inp of
  Err _ -> Pass () inp
  Pass _ _ -> Err [(Expect "end of input", pos)]

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

whtspc :: Parser s String
whtspc = expect (many $ anyof "\n\t ") "whitespace"

between :: Parser s a -> Parser s b -> Parser s c -> Parser s b
between lhs p rhs = lhs *> p <* rhs

wrap :: String -> Parser s a -> String -> Parser s a
wrap lhs p rhs = between (string lhs) p (string rhs)

strip :: Parser s a -> Parser s a
strip p = whtspc *> p <* whtspc

digit :: Parser s Int
digit = read . (:[]) <$> numeric

number :: Parser s Int
number = read <$> some numeric

-- TODO: add where clause to remove duplicate
(|=) :: (s -> s) -> Parser s s
(|=) fun = Parser $ \(stream, pos, state) -> Pass (fun state) (stream, pos, fun state)

(|-) :: Parser s s
(|-) = (|=) id 
