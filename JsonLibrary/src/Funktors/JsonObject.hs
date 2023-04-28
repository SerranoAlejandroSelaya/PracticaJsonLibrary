module Funktors.JsonObject where

import Data.Char
import Control.Applicative hiding (many)


data JValue a = JBool Bool
        |  JString String
        |  JObj [(String, (JValue a))]
        |  JNumber Double 
        |  JArray [(JValue a)]
        |  JNull 
        |  JToken (String, (JValue a))
        deriving (Show)

data Parser a = Parser {parse:: String -> Maybe(a,String)} 

instance Functor Parser where 
    -- fmap :: (a->b) -> f a -> f b
    fmap f (Parser a) = Parser (\x -> case a x of 
        Just(a, xs) ->  Just (f a, xs)
        Nothing -> Nothing
        )

instance Applicative Parser where
    --pure :: a -> Parser a
    pure a = Parser (\x -> Just(a, x))
    --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser f) <*> (Parser g) = Parser (\input -> case f input of
        Just (func, rest) -> case g rest of
            Just (val, rest') -> Just (func val, rest')
            Nothing -> Nothing
        Nothing -> Nothing)


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


charEnd :: Char -> Parser Char
charEnd c = Parser (\x -> case reverse x of 
                (y:ys) | y == c -> Just (c, reverse ys)
                _ -> Nothing)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = Parser $ \x ->
  case runParser p x of
    Just (y, ys) ->
      let rest = runParser (many' (sep *> p)) ys
      in case rest of
        Just (zs, zs') -> Just (y : zs, zs')
        Nothing -> Just ([y], ys)
    Nothing -> Nothing

parseValue :: Parser (JValue a)
parseValue = parseBool <|> parseString <|> parseNumber <|> parseArray <|> parseNull <|> parseObject
    where parseNull = Parser (\x -> case x of 
                                    'n':'u':'l':'l':rest -> Just (JNull, rest)  
                                    _ -> Nothing)


char :: Char -> Parser Char
char c = Parser (\input -> case input of
    x:xs | x == c -> Just (c, xs)
    _ -> Nothing)

parseList :: Parser [JValue a]
parseList = char '[' *> whitespace *>  sepBy parseValue ( char ',' *> whitespace) <* whitespace <* char ']'

parseArray :: Parser (JValue a)
parseArray = Parser (\x -> (case parse parseList x of
    Just (x, a) -> Just(JArray x, a)
    Nothing -> Nothing))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser p) input = p input

parseToken ::  Parser(String, JValue a)
parseToken = Parser (\x -> case x of
    xs -> let Just (JString key, rest) = runParser parseString xs
          in case runParser (whitespace *> char ':' *> whitespace *> parseValue) rest of
            Just (val, rest') -> Just ((key, val), rest')
            Nothing -> Nothing)

parseObj :: Parser [(String, JValue a1)]
parseObj = char '{' *> whitespace *>  sepBy parseToken (char ',' *> whitespace) <* whitespace <* char '}'


parseObject :: Parser (JValue a)
parseObject = Parser (\x -> (case parse parseObj x of
    Just (x, a) -> Just(JObj x, a)
    Nothing -> Nothing))


writeJson :: JValue a -> String
writeJson (JBool True) = "true"
writeJson (JBool False) = "false"
writeJson (JString s) = show s
writeJson (JNumber n) = show n
writeJson JNull = "null"
writeJson (JObj pairs) = "{" ++ intercalate "," (map pairToString pairs) ++ "}"
  where
    pairToString (key, val) = show key ++ ":" ++ writeJson val
writeJson (JArray vals) = "[" ++ intercalate "," (map writeJson vals) ++ "]"
writeJson (JToken (key, val)) = "{" ++ show key ++ ":" ++ writeJson val ++ "}"

intercalate :: [a] -> [[a]] -> [a]
intercalate sep [] = []
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

parseJson :: String -> String
parseJson "" = "Not is a Json"
parseJson "{}" = "{}"
parseJson  xs = let Just (asw,rest) = parse parseValue xs in if rest == "" then (show asw)  else "Not is a Json"

jsonJValue :: String -> String
jsonJValue "" = "Not is a Json"
jsonJValue "{}" = "{}"
jsonJValue xs = let Just (asw,rest) = parse parseValue xs in if rest == "" then (writeJson asw)  else "Not is a Json"

