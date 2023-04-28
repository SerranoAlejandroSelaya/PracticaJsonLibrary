module Main where

import Funktors.JsonObject
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "JSON Tests" [testParseJsonNull, testParseJsonBool, testParseJsonNumber, testParseJsonString, testParseJsonArray, testParseJsonObject,
 testParseNestedObjects, testParseObjectWithList, testParseListWithObject, testParseListWithNestedObjects, negativeTestParseListWithObject]


testParseJsonNull :: TestTree
testParseJsonNull = testCase "Parse Json Null" $
  parseJson "null" @?= "JNull"

testParseJsonBool :: TestTree
testParseJsonBool = testCase "Parse Json Boolean" $
  parseJson "true" @?= "JBool True"

testParseJsonNumber :: TestTree
testParseJsonNumber = testCase "Parse Json Number" $
  parseJson "42" @?= "JNumber 42.0"

testParseJsonString :: TestTree
testParseJsonString = testCase "Parse Json String" $
  parseJson "\"Hello, World!\"" @?= "JString \"Hello, World!\""

testParseJsonArray :: TestTree
testParseJsonArray = testCase "Parse Json Array" $
  parseJson "[1,2,3]" @?= "JArray [JNumber 1.0,JNumber 2.0,JNumber 3.0]"

testParseJsonObject :: TestTree
testParseJsonObject = testCase "Parse Json Object" $
  parseJson "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}" @?=
    "JObj [(\"name\",JString \"John\"),(\"age\",JNumber 30.0),(\"city\",JString \"New York\")]"

testParseNestedObjects :: TestTree
testParseNestedObjects = testCase "Parse JSON with nested objects" $
  parseJson "{\"name\":\"John\", \"age\":30, \"address\":{\"street\":\"Main St.\", \"number\":123}}" @?=
    "JObj [(\"name\",JString \"John\"),(\"age\",JNumber 30.0),(\"address\",JObj [(\"street\",JString \"Main St.\"),(\"number\",JNumber 123.0)])]"

testParseObjectWithList :: TestTree
testParseObjectWithList = testCase "Parse JSON Object with List" $
  parseJson "{\"name\":\"John\", \"age\":30, \"hobbies\":[\"reading\", \"cooking\", \"sports\"]}" @?=
    "JObj [(\"name\",JString \"John\"),(\"age\",JNumber 30.0),(\"hobbies\",JArray [JString \"reading\",JString \"cooking\",JString \"sports\"])]"

testParseListWithObject :: TestTree
testParseListWithObject = testCase "Parse JSON List with Object" $
  parseJson "[{\"name\":\"John\", \"age\":30}, {\"name\":\"Sarah\", \"age\":25}]" @?=
    "JArray [JObj [(\"name\",JString \"John\"),(\"age\",JNumber 30.0)],JObj [(\"name\",JString \"Sarah\"),(\"age\",JNumber 25.0)]]"

testParseListWithNestedObjects :: TestTree
testParseListWithNestedObjects = testCase "Parse JSON List with nested Objects" $
  parseJson "[{\"name\":\"John\", \"age\":30, \"address\":{\"street\":\"Main St.\", \"number\":123}}, {\"name\":\"Sarah\", \"age\":25, \"address\":{\"street\":\"Broadway\", \"number\":456}}]" @?=
    "JArray [JObj [(\"name\",JString \"John\"),(\"age\",JNumber 30.0),(\"address\",JObj [(\"street\",JString \"Main St.\"),(\"number\",JNumber 123.0)])],JObj [(\"name\",JString \"Sarah\"),(\"age\",JNumber 25.0),(\"address\",JObj [(\"street\",JString \"Broadway\"),(\"number\",JNumber 456.0)])]]"

negativeTestParseListWithObject :: TestTree
negativeTestParseListWithObject = testCase "Parse JSON List with Object" $
  parseJson "[{\"name\":\"John\", \"age\":30}, {\"name\":\"Sarah\", \"age\":25}]EstoNoDeberiaEstarAqui" @?=
    "Not is a Json"
