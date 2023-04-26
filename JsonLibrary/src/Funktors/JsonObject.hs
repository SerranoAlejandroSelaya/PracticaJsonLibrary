module JsonObject(JValue(..),NodeName) where 
import Data.Char
import Control.Applicative hiding (many)

data JValue a = JBool Bool
        |  JString String
        |  JObj [(String, Maybe (JValue a))]
        |  JNumber Double 
        |  JArray [(JValue a)]
        |  JNull 
        deriving (Show)

data Parser a = Parser {parse:: String -> Maybe(a,String)} 

instance Functor Parser where 
    -- fmap :: (a->b) -> f a -> f b
    fmap f (Parser a) = Parser (\x -> case a x of 
        Just(a, xs) ->  Just (f a, xs)
        Nothing -> Nothing
        )

instance Applicative Parser where
    -- pure :. a -> Parser
    pure a = Parser (\x -> Just(a, x))
    (Parser a) <*> (Parser b) = Parser (\x -> (do
        (a', rest) <- a x 
        (b', rest') <- b x
        Just(a' b',rest')))  
    (Parser p1) *> (Parser p2) = Parser $ \input -> do
        (_, input') <- p1 input
        p2 input'

instance Alternative Parser where
    empty = Parser(\_ -> Nothing)
    (Parser a) <|> (Parser b) = Parser(\input -> a input <|> b input)

parseBool :: Parser (JValue a) 
parseBool = Parser (\x ->  case x of 
    'f':'a':'l':'s':'e': xs -> Just (JBool (False),xs)
    't':'r':'u':'e': xs -> Just (JBool True,xs)
    _ -> Nothing) 

parseString :: Parser (JValue a) 
parseString = Parser(\x -> case x of 
    ('\"' : xs) -> Just (JString (takeWhile (\x -> (x == '\"') == False) xs) ,tail(dropWhile (\x -> (x == '\"') == False) xs) )
    _ -> Nothing
    )

parseNumber :: Parser (JValue a)
parseNumber = Parser( \s -> case span isDigit (skipChar s) of
  ("", _) -> Nothing
  (ds, rest) -> let (intPart, fracPart) = break (== '.') ds
                    numStr = intPart ++ fracPart
                    num = foldl (\a c -> a * 10 + fromIntegral (digitToInt c)) 0 numStr
                in Just (JNumber num, rest))

whitespace :: Parser ()
whitespace = Parser (\x -> Just ((), dropWhile isSpace x))
    where isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == ',' 

skipChar :: String -> String
skipChar x = let Just(_,xs) = runParser whitespace x in xs  

many' :: Parser a -> Parser [a]
many' p = Parser $ \s ->
    let step acc inp = case runParser p inp of
            Just (x, out) -> step (x : acc) out
            Nothing -> Just (reverse acc, inp)
    in step [] s


char' :: Char -> Parser Char
char' c = Parser (\x -> case x of 
                (y:ys) | y == c -> Just (c, ys)
                _ -> Nothing)


sepBy' :: Parser a -> Parser b -> Parser [a]
sepBy' p sep = Parser $ \x ->
  case runParser p x of
    Just (y, ys) ->
      let rest = runParser (many' (sep *> p)) ys
      in case rest of
        Just (zs, zs') -> Just (y : zs, zs')
        Nothing -> Just ([y], ys)
    Nothing -> Nothing


parseValue :: Parser (JValue a)
parseValue = parseBool <|> parseString <|> parseNumber <|> parseArray <|> parseNull
    where parseNull = Parser (\x -> case x of 
                                    'n':'u':'l':'l':rest -> Just (JNull, rest)  
                                    _ -> Nothing)
                                    
parseArray :: Parser (JValue a)
parseArray = char' '[' *> whitespace *> (JArray <$> sepBy' parseValue (char' ',' *> whitespace)) <* whitespace
