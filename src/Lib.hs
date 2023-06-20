module Lib (flattenJSON) where

import Data.Aeson (Object, Value (..))
import Data.Text (Text)
import Prelude

flattenJSON :: Value -> Value
flattenJSON (Object obj) = Object $ flattenObject obj ""
flattenJSON val = val

flattenObject :: Object -> Text -> Object
flattenObject obj prefix = obj
