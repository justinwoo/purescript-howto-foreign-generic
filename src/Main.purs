module Main where

import Prelude

import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.EnumEncoding (genericDecodeEnum, genericEncodeEnum)
import Foreign.Generic.Types (SumEncoding(..), Options)

newtype SimpleRecord = SimpleRecord
  { a :: Int
  , b :: String
  , c :: Boolean
  }
derive instance repGenericSimpleRecord :: Rep.Generic SimpleRecord _
derive instance eqSimpleRecord :: Eq SimpleRecord
instance showSimpleRecord :: Show SimpleRecord where
  show = genericShow
instance decodeSimpleRecord :: Decode SimpleRecord where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeSimpleRecord :: Encode SimpleRecord where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype NestedRecord = NestedRecord
  {  d :: SimpleRecord
  }
derive instance repGenericNestedRecord :: Rep.Generic NestedRecord _
derive instance eqNestedRecord :: Eq NestedRecord
instance showNestedRecord :: Show NestedRecord where
  show = genericShow
instance decodeNestedRecord :: Decode NestedRecord where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeNestedRecord :: Encode NestedRecord where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype RecordWithArrayAndNullOrUndefined = RecordWithArrayAndNullOrUndefined
  { intArray :: Array Int
  , optionalInt :: Maybe Int
  }
derive instance repGenericRecordWithArrayAndNullOrUndefined :: Rep.Generic RecordWithArrayAndNullOrUndefined _
derive instance eqRecordWithArrayAndNullOrUndefined :: Eq RecordWithArrayAndNullOrUndefined
instance showRecordWithArrayAndNullOrUndefined :: Show RecordWithArrayAndNullOrUndefined where
  show = genericShow
instance decodeRecordWithArrayAndNullOrUndefined :: Decode RecordWithArrayAndNullOrUndefined where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRecordWithArrayAndNullOrUndefined :: Encode RecordWithArrayAndNullOrUndefined where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

data Fruit
  = Apple
  | Banana
  | Watermelon
derive instance repGenericFruit :: Rep.Generic Fruit _
derive instance eqFruit :: Eq Fruit
instance showFruit :: Show Fruit where
  show = genericShow
-- since Fruit is an enum-style sum type (i.e. it's a sum type of constructors with no arguments, we can use genericDecodeEnum and genericEncodeEnum!)
instance decodeFruit :: Decode Fruit where
  decode = genericDecodeEnum { constructorTagTransform: identity }
instance encodeFruit :: Encode Fruit where
  encode = genericEncodeEnum { constructorTagTransform: identity }

newtype RecordWithADT = RecordWithADT
  { fruit :: Fruit
  }
derive instance repGenericRecordWithADT :: Rep.Generic RecordWithADT _
derive instance eqRecordWithADT :: Eq RecordWithADT
instance showRecordWithADT :: Show RecordWithADT where
  show = genericShow
instance decodeRecordWithADT :: Decode RecordWithADT where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRecordWithADT :: Encode RecordWithADT where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

data ADTWithArgs
  = Increment
  | Add Int
  | Set { count :: Int }
  | Reset
derive instance genericRepADTWithArgs :: Rep.Generic ADTWithArgs _
derive instance eqADTWithArgs :: Eq ADTWithArgs
instance showADTWithArgs :: Show ADTWithArgs where
  show = genericShow
instance decodeADTWithArgs :: Decode ADTWithArgs where
  decode = genericDecode defaultOptions
instance encodeADTWithArgs :: Encode ADTWithArgs where
  encode = genericEncode defaultOptions

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
typicalReduxActionOptions :: Options
typicalReduxActionOptions = defaultOptions
  { sumEncoding = TaggedObject
    { tagFieldName: "type"
    , contentsFieldName: "payload"
    , constructorTagTransform: identity
    }
  }
instance decodeTypicalReduxAction :: Decode TypicalJSTaggedObject where
  decode = genericDecode typicalReduxActionOptions
instance encodeTypicalReduxAction :: Encode TypicalJSTaggedObject where
  encode = genericEncode typicalReduxActionOptions
