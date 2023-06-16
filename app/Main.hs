module Main (main) where

import Control.Monad (forever)
import GHC.IO.Handle (hGetLine)
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, proc)
import Prelude

main :: IO ()
main = do
  putStrLn "Watching plist files..."
  let fswatchArgs = ["-r", "--include=.*\\.plist$", "--exclude=.*", "/"]
  (_, Just hout, _, _) <- createProcess (proc "fswatch" fswatchArgs) {std_out = CreatePipe}
  forever $ do
    path <- hGetLine hout
    printPlistFile path

printPlistFile :: FilePath -> IO ()
printPlistFile path = do
  putStrLn $ "Plist file changed: " ++ path
  return ()
