module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Main (ADTWithArgs(..), Fruit(..), NestedRecord(..), RecordWithADT(..), RecordWithArrayAndNullOrUndefined(..), SimpleRecord(..), TypicalJSTaggedObject(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

testJSON original input expected = do
  log' "can be converted to JSON"
    (show original) json
  it "can be converted back" $
    decodeJSON' json `shouldEqual` Right original
  it' "can be converted from JSON" input expected $
    decodeJSON' input `shouldEqual` expected
  where
    decodeJSON' = runExcept <<< decodeJSON
    json = encodeJSON $ original
    format a b c = a <> "\n    " <> b <> "\n    -> " <> c
    log' t a b = it (format t a b) $ pure unit
    it' a b c t = it (format a b $ show c) t

main = do
  run [consoleReporter] do
    describe "SimpleRecord" do
      testJSON
        (SimpleRecord { a: 1, b: "b", c: true })
        """{ "a": 123, "b": "abc", "c": false }"""
        (Right (SimpleRecord { a: 123, b: "abc", c: false }))

    describe "NestedRecord" do
      testJSON
        (NestedRecord { d: SimpleRecord { a: 1, b: "b", c: true }})
        """{ "d": { "a": 123, "b": "abc", "c": false } }"""
        (Right (NestedRecord { d: (SimpleRecord { a: 123, b: "abc", c: false })}))

    describe "RecordWithArrayAndNullOrUndefined" do
      testJSON
        (RecordWithArrayAndNullOrUndefined { intArray: [1, 2, 3] , optionalInt: wrap $ Just 1 })
        """{ "intArray": [1, 2, 3] }"""
        (Right (RecordWithArrayAndNullOrUndefined { intArray: [1, 2, 3] , optionalInt: wrap Nothing }))

    describe "Fruit - Enum style ADT" do
      testJSON
        (Apple)
        "\"Watermelon\""
        (Right Watermelon)

    describe "RecordWithADT" do
      testJSON
        (RecordWithADT { fruit: Apple })
        """{ "fruit": "Watermelon" }"""
        (Right (RecordWithADT { fruit: Watermelon }))

    describe "ADTWithArgs" do
      testJSON
        (Set { count: 5 })
        """{ "tag": "Add", "contents": 123 }"""
        (Right (Add 123))

    describe "TypicalJSTaggedObject" do
      testJSON
        (Login { username: "agent", password: "hunter2" })
        """{ "type": "Logout" }"""
        (Right (Logout))
