module Lib (convertPlistToHashMap) where

import Data.HashMap.Lazy qualified as HashMap
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Text.XML (Document, def, parseText)
import Text.XML.Cursor (content, element, followingSibling, fromDocument, ($//), (&/), (&|))
import Prelude

convertPlistToHashMap :: Text -> HashMap.HashMap Text Text
convertPlistToHashMap input =
  let doc = parseText def $ fromStrict input
      keyValuePairs = processDocument doc
   in HashMap.fromList keyValuePairs

processDocument :: Either a Document -> [(Text, Text)]
processDocument (Left _) = []
processDocument (Right doc) =
  let cursor = fromDocument doc
      keys = cursor $// element "key" &/ content
      values = map head $ cursor $// element "key" &| followingSibling &/ content
   in zip keys values
