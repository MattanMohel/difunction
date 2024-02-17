module Parser where

import Control.Applicative
import Data.Char

type State = (Int, Int, Int)
type Inp = (String, State)
type Out a = Either [(ParseErr, State)] (a, Inp)

data ParseErr
  = Expect String
  | Err String
  | EndOfInput
  | Empty

  deriving (Show)

newtype Parser a = Parser (Inp -> Out a)

instance Functor Parser where
  fmap fun p = Parser $ \inp -> case parse p inp of
    Right (out, inp) -> Right (fun out, inp)
    Left err -> Left err

instance Applicative Parser where
  pure a = Parser $ \inp -> Right (a, inp)
  pfun <*> p = Parser $ \inp -> case parse pfun inp of
    Right (fun, inp) -> parse (fmap fun p) inp
    Left err -> Left err

instance Monad Parser where
  p >>= fun = Parser $ \inp -> case parse p inp of
    Right (out, inp) -> parse (fun out) inp
    Left err -> Left err

instance Alternative Parser where
  empty = Parser $ \_ -> Left [(Empty, (0, 0, 0))]
  p <|> q = Parser $ \inp -> case parse p inp of
    Right out -> Right out
    Left errp -> case parse q inp of
      Right out -> Right out
      Left errq -> Left $ errp ++ errq

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
  Right out -> Right out
  Left _ -> Left [(Err mes, state)]

pass :: Monoid a => Parser a
pass = Parser $ \inp -> Right (mempty, inp)

consumed :: Parser a -> Parser a
consumed p = Parser $ \inp@(_, (_, _, idx)) -> case parse p inp of 
  Right out@(_, (_, state@(_, _, idx'))) -> if idx == idx' 
    then Left [(Err "didn't consume", state)]  
    else Right out
  Left err -> Left err

expect :: Parser a -> String -> Parser a
expect p mes = Parser $ \inp@(_, state) -> case parse p inp of
  Right out -> Right out
  Left _ -> Left [(Expect mes, state)]

item :: Parser Char
item = Parser $ \(stream, state) -> case stream of
  (x:xs) -> Right (x, (xs, advance x state))
  [] -> Left [(EndOfInput, state)]

sat :: (Char -> Bool) -> Parser Char
sat fun = item >>= \x -> if fun x then pure x else empty

char :: Char -> Parser Char
char c = expect (sat (==c)) [c]

range :: Char -> Char -> Parser Char
range a b = expect (sat $ \x -> x >= a && x <= b) (a:'-':b:"")

string :: String -> Parser String
string = foldr (\x -> (<*>) ((:) <$> char x)) (pure [])

eof :: Parser ()
eof = Parser $ \inp@(_, state) -> case parse item inp of
  Left _ -> Right ((), inp)
  Right _ -> Left [(Expect "end of input", state)]

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

token :: Parser a -> Parser a
token p = between whtspc p whtspc

digit :: Parser Int
digit = read . (:[]) <$> numeric

number :: Parser Int
number = read <$> some numeric

-- consumes no input on success
try :: Parser a -> Parser (Maybe a)
try p = Parser $ \inp -> case parse p inp of
  Left _ -> Right (Nothing, inp)
  Right (out, inp) -> Right (Just out, inp)
