module Main (main) where

import Control.Monad (forever)
import Data.Cache (Cache, insert, newCache)
import Data.Cache qualified as Cache
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.IO.Handle (hGetLine)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, proc, readProcessWithExitCode)
import Prelude

type PlistCache = Cache FilePath T.Text

main :: IO ()
main = do
  putStrLn "Watching plist files..."
  plistCache <- newCache Nothing :: IO PlistCache
  let fswatchArgs = ["-r", "--include=.*\\.plist$", "--exclude=.*", "/"]
  (_, Just hout, _, _) <- createProcess (proc "fswatch" fswatchArgs) {std_out = CreatePipe}
  forever $ do
    path <- hGetLine hout
    printPlistFile plistCache path

printPlistFile :: PlistCache -> FilePath -> IO ()
printPlistFile cache path = do
  putStrLn $ "Plist file changed: " ++ path
  previousContents <- Cache.lookup cache path
  (exitCode, currentContents) <- callPlistBuddy "Print" path
  case exitCode of
    ExitSuccess -> do
      case previousContents of
        Just _ -> do
          -- Update the cache with the new contents
          insert cache path currentContents
        Nothing -> do
          -- Add the file to the cache without generating PlistBuddy commands
          insert cache path currentContents
      return ()
    _ -> do
      TIO.putStrLn $ "Error reading plist file: " <> T.pack path <> " - " <> currentContents
      return ()

callPlistBuddy :: String -> FilePath -> IO (ExitCode, T.Text)
callPlistBuddy command path = do
  let plistBuddyArgs = ["-x", "-c", command, path]
  (exitCode, output, _) <- readProcessWithExitCode "/usr/libexec/PlistBuddy" plistBuddyArgs ""
  return (exitCode, T.pack output)
