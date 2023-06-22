module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Data.Aeson (Value (Bool, Number, String), decode, encode)
import Data.Aeson.KeyMap (toHashMapText)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Cache (Cache, insert, newCache)
import Data.Cache qualified as Cache
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import GHC.IO.Handle (hGetLine)
import Lib (flattenObject)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, proc, readProcess, readProcessWithExitCode)
import Prelude

type PlistCache = Cache FilePath (HashMap T.Text Value)

main :: IO ()
main = do
  putStrLn "Watching plist files..."
  plistCache <- newCache Nothing :: IO PlistCache
  let fswatchArgs = ["-r", "--include=.*\\.plist$", "--exclude=.*", "/"]
  (_, Just hout, _, _) <- createProcess (proc "fswatch" fswatchArgs) {std_out = CreatePipe}

  -- Create a separate thread to call `defaults read` every 0.1s
  -- We want to call `defaults read` periodically because it forces plist files to reflect any changes made to them.
  _ <- forkIO $ forever $ do
    _ <- readProcess "defaults" ["read"] ""
    threadDelay 100000 -- 0.1s in microseconds
  forever $ do
    path <- hGetLine hout
    printPlistFile plistCache path

printPlistFile :: PlistCache -> FilePath -> IO ()
printPlistFile cache path = do
  previousContents <- Cache.lookup cache path
  (exitCode, xmlData) <- callPlistBuddy "Print" path
  case exitCode of
    ExitSuccess -> do
      currentContents <- convertPlistToJSON xmlData
      case previousContents of
        Just oldContents -> do
          -- Find the updated, added, and deleted keys
          let updatedKeys = HashMap.keys $ HashMap.intersection currentContents oldContents
          let addedKeys = HashMap.keys $ HashMap.difference currentContents oldContents
          let deletedKeys = HashMap.keys $ HashMap.difference oldContents currentContents

          -- Generate and print the Set, Add, and Delete commands
          mapM_ (printSetCommand oldContents currentContents path) updatedKeys
          mapM_ (printAddCommand currentContents path) addedKeys
          mapM_ (printDeleteCommand path) deletedKeys

          -- Update the cache with the new contents
          insert cache path currentContents
        Nothing -> do
          -- Add the file to the cache without generating PlistBuddy commands
          insert cache path currentContents
      return ()
    _ -> do
      TIO.putStrLn $ "Error reading plist file: " <> T.pack path <> " - " <> xmlData
      return ()

plistBuddyPath :: T.Text
plistBuddyPath = "/usr/libexec/PlistBuddy"

callPlistBuddy :: String -> FilePath -> IO (ExitCode, T.Text)
callPlistBuddy command path = do
  let plistBuddyArgs = ["-x", "-c", command, path]
  (exitCode, output, _) <- readProcessWithExitCode (T.unpack plistBuddyPath) plistBuddyArgs ""
  return (exitCode, T.pack output)

convertPlistToJSON :: T.Text -> IO (HashMap T.Text Value)
convertPlistToJSON xmlInput = do
  jsonString <- T.pack <$> readProcess "node" ["index.js", T.unpack xmlInput] ""
  case decode (fromStrict $ encodeUtf8 jsonString) of
    Just obj -> return $ toHashMapText (flattenObject obj)
    Nothing -> return HashMap.empty

printSetCommand :: HashMap T.Text Value -> HashMap T.Text Value -> FilePath -> T.Text -> IO ()
printSetCommand oldContents currentContents path key =
  let oldValue = oldContents HashMap.! key
      newValue = currentContents HashMap.! key
   in when (oldValue /= newValue) $ TIO.putStrLn $ generateSetCommand key newValue $ T.pack path

printAddCommand :: HashMap T.Text Value -> FilePath -> T.Text -> IO ()
printAddCommand currentContents path key =
  let value = currentContents HashMap.! key
   in TIO.putStrLn $ generateAddCommand key value $ T.pack path

printDeleteCommand :: FilePath -> T.Text -> IO ()
printDeleteCommand path key =
  TIO.putStrLn $ generateDeleteCommand key $ T.pack path

valueTypeString :: Value -> String
valueTypeString value =
  case value of
    (String _) -> "string"
    (Number _) -> "integer" -- assuming integers for simplicity
    (Bool _) -> "bool"
    _ -> "unknown"

generateAddCommand :: T.Text -> Value -> T.Text -> T.Text
generateAddCommand key value path =
  let valueType = T.pack $ valueTypeString value
      valueText = T.pack (unpack (encode value))
   in plistBuddyPath <> " -c \"Add " <> key <> " " <> valueType <> " " <> valueText <> "\" " <> path

generateDeleteCommand :: T.Text -> T.Text -> T.Text
generateDeleteCommand key path =
  plistBuddyPath <> " -c \"Delete " <> key <> "\" " <> path

generateSetCommand :: T.Text -> Value -> T.Text -> T.Text
generateSetCommand key value path =
  plistBuddyPath <> " -c \"Set " <> key <> " " <> T.pack (unpack (encode value)) <> "\" " <> path
