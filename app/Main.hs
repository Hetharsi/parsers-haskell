module Main where

main :: IO ()
main = do
    return ()

newtype Parser a = Parser {runParser :: String -> [(a,String)]}

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \inp -> concat [runParser (f a) inp' | (a,inp') <- runParser p inp]

instance Applicative Parser where
    pure x = Parser (\inp -> [(x, inp)])
    Parser f <*> Parser g = Parser $ \inp -> let [(s, newInp)] = f inp
                                                 [(s2, newInp')] = g newInp'
                                             in [(s s2, newInp')]

instance Functor Parser where
    fmap f (Parser s) = Parser $ \inp -> let [(a, b)] = s inp in [(f a, b)]

-- Choise of combinators

class Monad m => MonadZero m where
    zero :: m a 

class MonadZero m => MonadPlus m where
    mAdd :: m a -> m a -> m a  

instance MonadZero Parser where
    zero = Parser $ \inp -> []

instance MonadPlus Parser where
    mAdd (Parser f) (Parser g) = Parser $ \inp -> 
        (runParser (Parser f) inp) ++ (runParser (Parser g) inp)

(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \cs -> case runParser (p `mAdd` q) cs of
    []     -> []
    (x:xs) -> [x]

testParser :: Parser String
testParser = Parser (\x -> [(take 2 x, x)])



item :: Parser Char
item  = Parser (\cs -> case cs of
    ""     -> []
    (c:cs) -> [(c,cs)])

sat  :: (Char -> Bool) -> Parser Char
sat p = do 
    c <- item 
    if p c then return c else zero

-- *Main> runParser (sat (=='c')) "asdf"
-- []
-- *Main> runParser (sat (=='c')) "clllsdf"
-- [('c',"lllsdf")]

char :: Char -> Parser Char
char c = sat (c==) 

string :: String -> Parser String
string [] = return ""
string xs = do
    return xs

many   :: Parser a -> Parser [a]
many p  = many1 p +++ return []

many1  :: Parser a -> Parser [a]
many1 p = do 
    a <- p
    as <- many p
    return (a:as)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep  = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do 
    a  <- p
    as <- many (do {sep; p})
    return (a:as)
