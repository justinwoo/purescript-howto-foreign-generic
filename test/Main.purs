module Test.Main where

import Prelude
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Class (class IsForeign, readJSON, write)
import Global.Unsafe (unsafeStringify)
import Main (ADTWithArgs(..), Fruit(..), NestedRecord(..), RecordWithADT(..), SimpleRecord(..), TypicalJSTaggedObject(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

readJSON' :: forall a. IsForeign a => String -> Either _ a
readJSON' = runExcept <<< readJSON

main = do
  run [consoleReporter] do
    describe "SimpleRecord" do
      it "can be converted to JSON and converted back" do
        let simpleRecord = SimpleRecord { a: 1, b: "b", c: true }
        let json = unsafeStringify <<< write $ simpleRecord
        log' json
        readJSON' json `shouldEqual` Right simpleRecord
      it "can be converted from JSON" do
        let simpleRecordJSON = "{ \"a\": 123, \"b\": \"abc\", \"c\": false }"
        readJSON' simpleRecordJSON
          `shouldEqual` Right (SimpleRecord { a: 123, b: "abc", c: false })

    describe "NestedRecord" do
      it "can be converted to JSON and converted back" do
        let nestedRecord = NestedRecord { d: SimpleRecord { a: 1, b: "b", c: true }}
        let json = unsafeStringify <<< write $ nestedRecord
        log' json
        readJSON' json `shouldEqual` Right nestedRecord
      it "can be converted from JSON" do
        let nestedRecordJSON = "{ \"d\": { \"a\": 123, \"b\": \"abc\", \"c\": false } }"
        readJSON' nestedRecordJSON
          `shouldEqual` Right (NestedRecord { d: (SimpleRecord { a: 123, b: "abc", c: false })})

    describe "RecordWithADT" do
      it "can be converted to JSON and converted back" do
        let recordWithADT = RecordWithADT { fruit: Apple }
        let json = unsafeStringify <<< write $ recordWithADT
        log' json
        readJSON' json `shouldEqual` Right recordWithADT
      it "can be converted from JSON" do
        let recordWithADTJSON = "{ \"fruit\": \"Watermelon\" }"
        readJSON' recordWithADTJSON
          `shouldEqual` Right (RecordWithADT { fruit: Watermelon })

    describe "ADTWithArgs" do
      it "can be converted to JSON and converted back" do
        let adtWithArgs = Set { count: 5 }
        let json = unsafeStringify <<< write $ adtWithArgs
        log' json
        readJSON' json `shouldEqual` Right adtWithArgs
      it "can be converted from JSON" do
        let adtWithArgsJSON = "{ \"tag\": \"Add\", \"contents\": 123 }"
        readJSON' adtWithArgsJSON
          `shouldEqual` Right (Add 123)

    describe "TypicalJSTaggedObject" do
      it "can be converted to JSON and converted back" do
        let typicalJSTaggedObject = Login { username: "agent", password: "hunter2" }
        let json = unsafeStringify <<< write $ typicalJSTaggedObject
        log' json
        readJSON' json `shouldEqual` Right typicalJSTaggedObject
      it "can be converted from JSON" do
        let typicalJSTaggedObjectJSON = "{ \"type\": \"Logout\" }"
        readJSON' typicalJSTaggedObjectJSON
          `shouldEqual` Right (Logout)

  where
    log' = log <<< append "\n"

