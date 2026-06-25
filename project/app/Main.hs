module Main (main) where

import Lib
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> runOnSheet filePath
    _ -> do
      putStrLn "Usage: stack run -- <spreadsheet-file-path>"
      putStrLn ""
      let dir = "spreadsheets"
      exists <- doesDirectoryExist dir
      when exists $ do
        putStrLn "Available spreadsheets in 'spreadsheets/':"
        files <- listDirectory dir
        mapM_ (\f -> putStrLn $ "  " ++ dir ++ "/" ++ f) files

runOnSheet :: FilePath -> IO ()
runOnSheet filePath = do
  exists <- doesFileExist filePath
  if not exists
    then putStrLn $ "Error: File '" ++ filePath ++ "' does not exist."
    else do
      putStrLn $ "Reading " ++ filePath ++ "..."
      fileContent <- readFile filePath
      case parseSheet fileContent of
        Left err -> do
          putStrLn "Parsing failed!"
          putStrLn err
        Right sheet -> do
          putStrLn "Parsing successful! Evaluating sheet..."
          let env = evalSheet sheet
          putStrLn "\nResulting Environment:"
          mapM_ (\(addr', val) -> putStrLn $ show addr' ++ " = " ++ show val) (Map.toList env)
