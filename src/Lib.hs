module Lib (flattenObject) where

import Data.Aeson (Object, Value (Object))
import Data.Aeson.KeyMap (fromHashMapText, toHashMapText)
import Data.HashMap.Strict (fromList, toList)
import Data.Text qualified as T
import Prelude

flattenObject :: Object -> Object
flattenObject = flattenObject' ""

flattenObject' :: T.Text -> Object -> Object
flattenObject' prefix obj = fromHashMapText $ fromList $ concatMap process $ toList $ toHashMapText obj
  where
    process (key, value) = case value of
      Object nested -> toList $ toHashMapText $ flattenObject' (prefix <> ":" <> key) nested
      _ -> [(prefix <> ":" <> key, value)]
