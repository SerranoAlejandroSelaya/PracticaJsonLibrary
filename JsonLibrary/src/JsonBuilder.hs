module JsonBuilder(writeJson, parseJson) where

import JsonObject
import Data.List
import Data.Char
import qualified Data.Maybe

showNode :: (String,Maybe JValue) -> String 
showNode (name, (Just value)) = name ++ ": " ++ writeJson value

createFromObject :: [(String, Maybe JValue)] -> String
createFromObject [] = ""
createFromObject xs = intercalate "," (map showNode xs)

writeJson :: JValue -> String 
writeJson ( (JBool b)) = show b
writeJson ( (JString s)) = show s
writeJson ( (JNumber d )) = show d
writeJson ( (JArray a)) = "[ " ++ intercalate ", " (map show a) ++ " ]"
writeJson ( (JObj o)) = "{" ++ createFromObject o ++ "}"

parseBool :: String -> Maybe JValue 
parseBool ['f','a','l','s','e', xs] = Just (JBool False)
parseBool ['t','r','u','e', xs] = Just (JBool True)
parseBool ['F','a','l','s','e', xs] = Just (JBool False)
parseBool ['T','r','u','e', xs] = Just (JBool True)
parseBool ['f','a','l','s','e'] = Just (JBool False)
parseBool ['t','r','u','e'] = Just (JBool True)
parseBool ['F','a','l','s','e'] = Just (JBool False)
parseBool ['T','r','u','e'] = Just (JBool True)
parseBool _ = Nothing 

parseNumber::String -> Maybe JValue
parseNumber n = if isDigit' n then Just (JNumber (read n)) else Nothing 

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' [x] = isDigit x
isDigit' ('-':xs) = isDigit' xs
isDigit' (x:xs) = (isDigit x) && (isDigit' xs) 

splitNodes :: Char -> [Char]-> [String]
splitNodes _ [] = []
splitNodes a xs = (splitAcc a True(Just xs) [])

splitAcc :: Char -> Bool -> Maybe String -> String -> [String]
splitAcc _ _ Nothing [] = []
splitAcc _ _ (Just []) [] = []
splitAcc _ _ (Just []) ys = [reverse ys]
splitAcc c b (Just(x:xs)) ys
            | (x == '[') = splitAcc c False (Just xs) (x : ys)
            | (x == ']') = splitAcc c True (Just xs) (x : ys)
            | (x == '{') = splitAcc c False (Just xs) (x : ys)
            | (x == '}') = splitAcc c True (Just xs) (x : ys)
            | (x == c && b) = reverse ys : splitAcc c b (Just xs) []
            | otherwise = splitAcc c b (Just xs) (x : ys)

buildTupple :: String -> (String, Maybe JValue)
buildTupple [] = ("", Just (JString ""))
buildTupple s  = (trim x, getJVal (trim y)) where x:(y:ys)= (splitNodes ':' s)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

isOpenL :: String -> Bool
isOpenL ('[': _) = True 
isOpenL _ = False

isOpenObject :: String -> Bool
isOpenObject ('{': _) = True 
isOpenObject _ = False

parseString :: String -> Maybe JValue
parseString "" = Nothing
parseString ('\"' : xs) = Just (JString (takeWhile (\x -> (x == '\"') == False) xs))
parseString _ = Nothing

parseList :: String -> Maybe JValue
parseList [] = Nothing
parseList xs = Just (JArray (parseList' xs))

parseList' :: String -> [Maybe JValue]
parseList' [] = [Just JNull]
parseList' xs 
    | isOpenL xs == True = map getJVal (splitNodes ',' (tail (init (xs))))
    | otherwise = [Nothing]

parseObj :: String -> Maybe JValue
parseObj [] = Nothing
parseObj xs = Just (JObj (parseObj' xs))

parseObj' :: String -> [(String, Maybe JValue )]
parseObj' [] = [("",Just JNull)]
parseObj' xs = if isOpenObject xs
    then map buildTupple (splitNodes ',' (tail (init xs)))
    else [("",Nothing)]

parseJson :: String -> Maybe JValue
parseJson [] = Nothing
parseJson xs 
        | isOpenObject xs == True =  (parseObj xs)
        | isOpenL xs == True =  (parseList xs)
        | otherwise = Nothing

getJVal :: String -> Maybe JValue
getJVal xs 
            |Data.Maybe.isJust (parseBool xs) = parseBool xs
            |Data.Maybe.isJust (parseNumber xs) = parseNumber xs
            |Data.Maybe.isJust (parseString xs) = parseString xs
            |Data.Maybe.isJust (parseJson xs) = parseJson xs
            |otherwise = Nothing

