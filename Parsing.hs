-- Functional parsing library from chapter 8 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2007.
-- Modified by Bruno Oliveira

module Parsing where
 
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 +++

-- Basic parsers
data Parser a =  P (String -> [(a,String)])

-- The monad of parsers (for the do-notation)

instance Functor Parser where
   fmap f (P g) = P (\s -> fmap (\(x,s') -> (f x, s')) (g s))

instance Applicative Parser where
   pure   = return
   (<*>)  = ap

instance Alternative Parser where
   empty = failure
   (<|>) = (+++)

instance Monad Parser where
   return v =  P (\inp -> [(v,inp)])
   p >>= f  =  P (\inp -> case parse p inp of
                             []        -> []
                             [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero        =  P (\inp -> [])
   p `mplus` q  =  P (\inp -> case parse p inp of
                                 []        -> parse q inp
                                 [(v,out)] -> [(v,out)])

-- parsing functions

failure :: Parser a
failure = P (\inp -> []) 

-- String -> [(Char,String)]

item :: Parser Char
item =  P (\inp ->
   case inp of
      ""      -> []
      (c:cs)  -> [(c,cs)]
   )


parse :: Parser a -> String -> [(a,String)]
parse (P p) inp  =  p inp


-- String -> [(Char,String)]
-- Choice: tries p first and if p fails it tries q
-- Hint: use "parser"

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  P (\inp ->
  case parse p inp of
    []     -> parse q inp
    _      -> parse p inp)
-- parse inp

-- String -> [(Char,String)]

-- Example:

p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x,y)

-- Derived primitives

-- Parsing a character that satisfies a predicate
-- Hint: Use the do-notation and the primitive parsers (item, return, failure)

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then
            return c
           else failure

-- parse a digit (example: 1, 2, 9, ...). Note: can use isDigit :: Char -> Bool

char :: Char -> Parser Char
char c = sat (c ==)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower =  sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter =  sat isAlpha

alphanum :: Parser Char
alphanum =  sat isAlphaNum

--char :: Char -> Parser Char
--char x =  sat (== x)

-- string "if"
-- string "then"

string :: String -> Parser String
string "" = return ""
string (c:cs) = 
  do x <- char c
     xs <- string cs
     return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- Apply the parser at least one time
many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)







-- many1 :: Parser a -> Parser [a]
-- many1 p =  do v  <- p
--              vs <- many p
--              return (v:vs)

ident :: Parser String
ident =  do x  <- lower
            xs <- many alphanum
            return (x:xs)

-- Example: parsing a list of one or more digits 
-- from a string

-- [1,2,3]

dList :: Parser String
dList  = do char '['
            d  <- digit
            ds <- many (do char ','
                           digit)
            char ']'
            return (d:ds)

-- Parsing numbers and space

nat :: Parser Int
nat =  do xs <- many1 digit
          return (read xs)

int :: Parser Int
int =  do char '-'
          n <- nat
          return (-n)
        +++ nat

space :: Parser ()
space =  do many (sat isSpace)
            return ()

-- Ignoring spacing

token  :: Parser a -> Parser a
token p =  do space
              v <- p
              space
              return v

-- pString :: Parser [a]
-- pString = do space
--              x <- 

identifier  :: Parser String
identifier  =  token ident

natural :: Parser Int
natural =  token nat

integer :: Parser Int
integer =  token int

symbol :: String -> Parser String
symbol xs =  token (string xs)

firstAlpha :: Parser Char
firstAlpha = do many (sat (not . isAlpha))
                item

manyN :: Int -> Parser a -> Parser [a]
manyN 0 p = return []
manyN n p = do x <- p
               xs <- manyN (n-1) p
               return (x:xs)

-- expr -> term ('+' expr | e)
-- expr :: Parser Int
-- expr = do x <- term
--           (do char '+'
--               y <- expr
--               return (x+y)) +++ return x
-- term -> factor ('*' term | e)
-- term :: Parser Int
-- term = do f <- factor
--           (do char '*'
--               t <- term
--               return (f * t)) +++ return f
-- factor -> '('expr')' | digit
-- Hint : use natural for parsing digits
-- factor :: Parser Int
-- factor = natural +++
--           (do char '('
--               x <- expr
--               char ')'
--               return x)

-- eval :: String -> Int
-- eval xs = case (parse expr xs) of
--               [(n,[])] -> n
--               [(_,out)] -> error "unsed"
