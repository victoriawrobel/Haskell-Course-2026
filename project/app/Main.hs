module Main (main) where

import Lib
import qualified Data.Map as Map

main :: IO ()
main = do
  let sheetText = unlines
        [ "sheet {"
        , "  A1 = 10;"
        , "  A2 = 20;"
        , "  A3 = A1 + A2;"
        , "  A4 = SUM(A1:A3);"
        , "}"
        ]
  putStrLn "Welcome to SpreadsheetLang!"
  putStrLn "Parsing example sheet:"
  putStrLn sheetText
  case parseSheet sheetText of
    Left err -> putStrLn $ "Parsing error: " ++ err
    Right sheet -> do
      putStrLn "Parsing successful! Evaluating sheet..."
      let env = evalSheet sheet
      putStrLn "Resulting Environment:"
      mapM_ (\(addr', val) -> putStrLn $ show addr' ++ " = " ++ show val) (Map.toList env)
