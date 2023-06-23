module Lib (flattenObject) where

import Data.Aeson (Object, Value (Array, Object))
import Data.Aeson.KeyMap (fromHashMapText, toHashMapText)
import Data.Foldable (toList)
import Data.HashMap.Strict (fromList)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Prelude

flattenObject :: Object -> Object
flattenObject = flattenObject' ""

flattenObject' :: T.Text -> Object -> Object
flattenObject' prefix obj = fromHashMapText $ fromList $ concatMap process $ HashMap.toList $ toHashMapText obj
  where
    process :: (T.Text, Value) -> [(T.Text, Value)]
    process (key, value) = case value of
      Object nested -> HashMap.toList $ toHashMapText $ flattenObject' (prefix <> ":" <> key) nested
      Array arr -> concatMap (\(i, v) -> process (key <> ":" <> T.pack (show i), v)) (zip [0 :: Int ..] (toList arr))
      _ -> [(prefix <> ":" <> key, value)]
