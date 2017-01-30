module Main where

import Prelude
import Data.Generic.Rep as Rep
import Data.Foreign (ForeignError(..), fail, readString, toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Foreign.Generic.Types (SumEncoding(..))
import Data.Generic.Rep.Show (genericShow)

newtype SimpleRecord = SimpleRecord
  { a :: Int
  , b :: String
  , c :: Boolean
  }
derive instance repGenericSimpleRecord :: Rep.Generic SimpleRecord _
derive instance eqSimpleRecord :: Eq SimpleRecord
instance showSimpleRecord :: Show SimpleRecord where
  show = genericShow
instance isForeignSimpleRecord :: IsForeign SimpleRecord where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignSimpleRecord :: AsForeign SimpleRecord where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}

newtype NestedRecord = NestedRecord
  {  d :: SimpleRecord
  }
derive instance repGenericNestedRecord :: Rep.Generic NestedRecord _
derive instance eqNestedRecord :: Eq NestedRecord
instance showNestedRecord :: Show NestedRecord where
  show = genericShow
instance isForeignNestedRecord :: IsForeign NestedRecord where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignNestedRecord :: AsForeign NestedRecord where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}

data Fruit
  = Apple
  | Banana
  | Watermelon
derive instance repGenericFruit :: Rep.Generic Fruit _
derive instance eqFruit :: Eq Fruit
instance showFruit :: Show Fruit where
  show = genericShow
instance isForeignFruit :: IsForeign Fruit where
  read x = do
    string <- readString x
    case string of
      "Apple" -> pure Apple
      "Banana" -> pure Banana
      "Watermelon" -> pure Watermelon
      _ -> fail $ ForeignError "We don't know what fruit this is!!!"
instance asForeignFruit :: AsForeign Fruit where
  write = toForeign <<< show
  -- write Apple = toForeign "Apple"
  -- write Banana = toForeign "Banana"
  -- write Watermelon = toForeign "Watermelon"


newtype RecordWithADT = RecordWithADT
  { fruit :: Fruit
  }
derive instance repGenericRecordWithADT :: Rep.Generic RecordWithADT _
derive instance eqRecordWithADT :: Eq RecordWithADT
instance showRecordWithADT :: Show RecordWithADT where
  show = genericShow
instance isForeignRecordWithADT :: IsForeign RecordWithADT where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignRecordWithADT :: AsForeign RecordWithADT where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}

data ADTWithArgs
  = Increment
  | Add Int
  | Set { count :: Int }
  | Reset
derive instance genericRepADTWithArgs :: Rep.Generic ADTWithArgs _
derive instance eqADTWithArgs :: Eq ADTWithArgs
instance showADTWithArgs :: Show ADTWithArgs where
  show = genericShow
instance isForeignADTWithArgs :: IsForeign ADTWithArgs where
  read = readGeneric defaultOptions
instance asForeignADTWithArgs :: AsForeign ADTWithArgs where
  write = toForeignGeneric defaultOptions

data TypicalJSTaggedObject
  = Logout
  | Login
    { username :: String
    , password :: String
    }
derive instance genericRepTypicalReduxAction :: Rep.Generic TypicalJSTaggedObject _
derive instance eqTypicalReduxAction :: Eq TypicalJSTaggedObject
instance showTypicalReduxAction :: Show TypicalJSTaggedObject where
  show = genericShow
typicalReduxActionOptions = defaultOptions
  { sumEncoding = TaggedObject
    { tagFieldName: "type"
    , contentsFieldName: "payload"
    }
  }
instance isForeignTypicalReduxAction :: IsForeign TypicalJSTaggedObject where
  read = readGeneric typicalReduxActionOptions
instance asForeignTypicalReduxAction :: AsForeign TypicalJSTaggedObject where
  write = toForeignGeneric typicalReduxActionOptions