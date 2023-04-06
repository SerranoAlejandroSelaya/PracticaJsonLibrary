module JsonObject(JValue(..),NodeName) where 

type  NodeName = String

data JValue = JBool Bool
          |  JString String
          |  JObj [(String, Maybe JValue )]
          |  JNumber Double 
          |  JArray [Maybe JValue]
          |  JNull 
          deriving (Show)

valBool :: JValue -> Bool
valBool (JBool a) = a 

valString :: JValue -> String 
valString (JString s) = s

valNumber :: JValue -> Double
valNumber (JNumber n) = n
