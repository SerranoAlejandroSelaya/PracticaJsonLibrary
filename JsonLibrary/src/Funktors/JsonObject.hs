module JsonObject(JValue(..),NodeName) where 
import Text.ParserCombinators.ReadP
import Data.Char

type  NodeName = String

data JValue a = JBool Bool
        |  JString String
        |  JObj [(String, Maybe (JValue a))]
        |  JNumber Double 
        |  JArray [Maybe (JValue a)]
        |  JNull 
        deriving (Show)

valBool :: JValue a -> Bool
valBool (JBool a) = a 

valString :: JValue a -> String 
valString (JString s) = s

valNumber :: JValue a -> Double
valNumber (JNumber n) = n

data Parser a = Parser {parse:: String -> Maybe(a,String)} 

parseBool :: Parser Bool 
parseBool = Parser (\x ->  case x of 
    'f':'a':'l':'s':'e': xs -> Just (False,xs)
    't':'r':'u':'e': xs -> Just (True,xs)
    _ -> Nothing) 

instance Functor Parser where 
    -- fmap :: (a->b) -> f a -> f b
    fmap f (Parser a) = Parser (\x -> case a x of 
        Just(a, xs) ->  Just (f a, xs)
        Nothing -> Nothing
        )

parseString :: Parser String 
parseString = Parser(\x -> case x of 
    ('\"' : xs) -> Just ((takeWhile (\x -> (x == '\"') == False) xs) ,tail(dropWhile (\x -> (x == '\"') == False) xs) )
    _ -> Nothing
    )

parseNumber :: Parser Double 
parseNumber = Parser(\x -> case x of 
    n -> let (num, str) = getCharDig n "" in if isDigit' num then Just ((read num), str) else Nothing )

getCharDig :: String -> String -> (String, String)
getCharDig "" acc = ("",acc)
getCharDig (x:xs) acc
            | (isDigit' [x]) = getCharDig (xs) (acc ++ [x])
            | x == '.' = getCharDig(xs) (acc ++ [x])
            | otherwise = (acc,x:xs) 

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' [x] = isDigit x
isDigit' ('.':xs) = isDigit' xs
isDigit' (x:xs) = (isDigit x) && (isDigit' xs) 
{-
Class 17-abr

Alicative:

data Maybe a = Nothing | Just 

instance Aplicative Maybe' where
    -- pure :: a -> Maybe a 
    pure x = Just' x
    -- <*> :: Maybe(a -> b) -> Maybe a-> Maybe b
    <*> Nothing' _ = Nothing'
    <*> (Just' f) (Just' a) = Just' (f a) 

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
    fmap f (Just x) = Just (f x)
-}


