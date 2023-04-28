module Funktors.Main (main) where
import JsonBuilder
import JsonObject 

main :: IO ()
main = print ((parseJson strJson5) )



jsonValue = JObj[("daysOfWeek", (Just (JArray[(Just (JString "t")),(Just(JString "M"))]))), ("Age",Just (JNumber 3.1416) )]

strJson  = "{\"a\":10,\"isTrue\":true ,\"name\":\"Jhon\", \"List\" : [\"uno\",\"dos\"]}"
strJson1 = "{\"a\":10,\"isTrue\":true ,\"name\":\"Jhon\", \"Obj\" : {\"uno\":\"dos\"}}"
strJson2 = "{\"a\":10,\"isTrue\":true ,\"name\":\"Jhon\", \"List\" : [\"uno\",[\"dos\",2]]}"
strJson3 = "{\"a\":10,\"isTrue\":true ,\"name\":\"Jhon\", \"Obj\" : {\"List\":[True,False]}}"
strJson4 = "{\"a\":10,\"isTrue\":true ,\"name\":\"Jhon\", \"List\" : [\"Obj\",{\"List\":[1,2,3]}]}"
strJson5 = "{\"a\":10,\"isTrue\":true, \"Obj\":{Obj:{Obj:{Num:1, list:[2,3,4]}}}}"




