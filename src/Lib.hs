module Lib (convertPlistToHashMap) where

import Data.HashMap.Lazy qualified as HashMap
import Data.Text (Text)

convertPlistToHashMap :: Text -> HashMap.HashMap Text Text
convertPlistToHashMap _ = HashMap.empty
