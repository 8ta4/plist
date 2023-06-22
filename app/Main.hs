module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Aeson (Value, decode)
import Data.Aeson.KeyMap (toHashMapText)
import Data.ByteString.Lazy (fromStrict)
import Data.Cache (Cache, insert, newCache)
import Data.Cache qualified as Cache
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import GHC.IO.Handle (hGetContents, hGetLine)
import Lib (flattenObject)
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStrLn)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), createProcess, proc, readProcess, readProcessWithExitCode)
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
  (exitCode, xmlData) <- callPlistBuddy True "Print" path
  case exitCode of
    ExitSuccess -> do
      currentContents <- convertPlistToJSON xmlData
      case previousContents of
        Just oldContents -> do
          -- Find the updated, added, and deleted keys
          let updatedKeys =
                filter
                  (\key -> HashMap.lookup key currentContents /= HashMap.lookup key oldContents)
                  (HashMap.keys $ HashMap.intersection currentContents oldContents)
          let addedKeys = HashMap.keys $ HashMap.difference currentContents oldContents
          let deletedKeys = HashMap.keys $ HashMap.difference oldContents currentContents

          -- Generate and print the Set, Add, and Delete commands
          mapM_ (printSetCommand path) updatedKeys
          mapM_ (printAddCommand path) addedKeys
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

callPlistBuddy :: Bool -> T.Text -> FilePath -> IO (ExitCode, T.Text)
callPlistBuddy useXML command path = do
  let plistBuddyArgs = (if useXML then ("-x" :) else id) ["-c", T.unpack command, path]
  (exitCode, output, _) <- readProcessWithExitCode (T.unpack plistBuddyPath) plistBuddyArgs ""
  return (exitCode, T.strip $ T.pack output)

plistBuddyPath :: T.Text
plistBuddyPath = "/usr/libexec/PlistBuddy"

convertPlistToJSON :: T.Text -> IO (HashMap T.Text Value)
convertPlistToJSON xmlInput = do
  jsonString <- T.pack <$> readProcess "node" ["index.js", T.unpack xmlInput] ""
  case decode (fromStrict $ encodeUtf8 jsonString) of
    Just obj -> return $ toHashMapText (flattenObject obj)
    Nothing -> return HashMap.empty

printAddCommand :: FilePath -> T.Text -> IO ()
printAddCommand path key = do
  (exitCode, currentValue) <- callPlistBuddy False ("Print " <> key) path
  case exitCode of
    ExitSuccess -> do
      xmlOutput <- callPlistBuddy True ("Print " <> key) path
      valueType <- getValueType (snd xmlOutput)
      TIO.putStrLn $ plistBuddyPath <> " -c \"Add " <> key <> " " <> valueType <> " " <> currentValue <> "\" " <> T.pack path
    _ -> do
      putStrLn $ "Error getting current value for key: " <> T.unpack key

printDeleteCommand :: FilePath -> T.Text -> IO ()
printDeleteCommand path key =
  TIO.putStrLn $ plistBuddyPath <> " -c \"Delete " <> key <> "\" " <> T.pack path

printSetCommand :: FilePath -> T.Text -> IO ()
printSetCommand path key = do
  (exitCode, currentValue) <- callPlistBuddy False ("Print " <> key) path
  case exitCode of
    ExitSuccess -> TIO.putStrLn $ plistBuddyPath <> " -c \"Set " <> key <> " " <> currentValue <> "\" " <> T.pack path
    _ -> putStrLn $ "Error getting current value for key: " <> T.unpack key

getValueType :: T.Text -> IO T.Text
getValueType xmlInput = do
  let yqCommand = "yq -p=xml '.plist | keys | .[1]'"
  (Just stdinYq, Just stdoutYq, _, _) <- createProcess (proc "sh" ["-c", T.unpack yqCommand]) {std_in = CreatePipe, std_out = CreatePipe}
  hPutStrLn stdinYq (T.unpack xmlInput)
  hClose stdinYq
  output <- T.strip . T.pack <$> hGetContents stdoutYq
  if output == "true" || output == "false"
    then return "bool"
    else return output
