module Parse where

import Data.Char (isDigit, isLower, isSpace, isUpper)
import Prelude hiding (pure)
import Debug.Trace (traceShow)

infixr 3 .|.
infixl 4 .*.
infixl 5 ..*
infixl 6 *..

type Parser a = String -> [(a,String)]

pure :: a -> Parser a
pure a = \s -> [(a,s)]

sat :: (Char -> Bool) -> Parser Char
sat f ""     =  []
sat f (c:s)  =  [(c, s) | f c]

char :: Char -> Parser Char
char c  =  sat (== c)

digit :: Parser Char
digit  =  sat isDigit

lower :: Parser Char
lower  =  sat isLower

upper :: Parser Char
upper  =  sat isUpper

(.*.) :: Parser (a -> b) -> Parser a -> Parser b
pf .*. px  =  \s -> [(f x, s'') | (f, s') <- pf s, (x, s'') <- px s']

string :: String -> Parser String
string ""      =  pure ""
string (c:cs)  =  pure (:) .*. char c .*. string cs

(*..) :: Parser a -> Parser b -> Parser b
px *.. py  =  pure (\x y -> y) .*. px .*. py

(..*) :: Parser a -> Parser b -> Parser a
px ..* py  =  pure (\x y -> x) .*. px .*. py

(.||.) :: Parser a -> Parser a -> Parser a
a .||. b  =  \s -> a s ++ b s

first :: Parser a -> Parser a
first p  =  \s -> take 1 (p s)

(.|.) :: Parser a -> Parser a -> Parser a
a .|. b  =  first (a .||. b)

many :: Parser a -> Parser [a]
many p  =  many1 p .|. pure []

many1 :: Parser a -> Parser [a]
many1 p  =  pure (:) .*. p .*. many p

spaces :: Parser String
spaces  =  many (sat isSpace)

token :: Parser a -> Parser a
token p  =  p ..* spaces

sym :: String -> Parser String
sym  =  token . string

infixl 4 .:.
(.:.) :: (a -> b) -> Parser a -> Parser b
f .:. p  =  pure f .*. p

infixl 4 ...
(...) :: a -> Parser b -> Parser a
x ... p  =  const x .:. p

satisfying :: Parser a -> (a -> Bool) -> Parser a
px `satisfying` f  =  \s -> [(x, s') | (x, s') <- px s, f x]

parseWith :: Parser a -> String -> Maybe a
parseWith p s  =  case [y | (y,"") <- p s] of
                  []   ->  Nothing
                  y:_  ->  Just y

-- straw man only

infixl 4 .+.

(.+.) :: Parser a -> Parser b -> Parser (a,b)
px .+. py    =  \s -> [ ((x,y), s'')
                      | (x, s') <- px s, (y, s'') <- py s']

using :: Parser a -> (a->b) -> Parser b
p `using` f  =  \s -> [ (f x, s')
                      | (x, s') <- p s]

string2 :: String -> Parser String
string2 ""      =  pure ""
string2 (c:cs)  =  (char c .+. string2 cs) `using` uncurry (:)

