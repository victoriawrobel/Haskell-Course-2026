module ParserSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Lib

spec :: Spec
spec = describe "Parser Correctness" $ do
  it "parses numeric, boolean, string, and negative literals" $ do
    let sheetStr = "sheet { A1 = 12.34; A2 = -5.6; B1 = TRUE; B2 = FALSE; C1 = \"hello\"; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right (Sheet m) -> do
        Map.lookup ("A", 1) m `shouldBe` Just (Cell ("A", 1) (Lit (NumV 12.34)))
        Map.lookup ("A", 2) m `shouldBe` Just (Cell ("A", 2) (Lit (NumV (-5.6))))
        Map.lookup ("B", 1) m `shouldBe` Just (Cell ("B", 1) (Lit (BoolV True)))
        Map.lookup ("B", 2) m `shouldBe` Just (Cell ("B", 2) (Lit (BoolV False)))
        Map.lookup ("C", 1) m `shouldBe` Just (Cell ("C", 1) (Lit (StrV "hello")))

  it "respects operator precedence (+ vs *)" $ do
    let sheetStr = "sheet { A1 = 10 + 2 * 3; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right (Sheet m) -> do
        case Map.lookup ("A", 1) m of
          Just (Cell _ (Form expr)) ->
            expr `shouldBe` BinOp Add (LitE (NumV 10)) (BinOp Mul (LitE (NumV 2)) (LitE (NumV 3)))
          _ -> expectationFailure "Expected A1 to be a Formula"

  it "parses range expressions (SUM, AVG)" $ do
    let sheetStr = "sheet { A1 = SUM(B1:B10); A2 = AVG(C1:D5); }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right (Sheet m) -> do
        case Map.lookup ("A", 1) m of
          Just (Cell _ (Form expr)) -> expr `shouldBe` RangeOp SumR ("B", 1) ("B", 10)
          _ -> expectationFailure "Expected SUM expression"
        case Map.lookup ("A", 2) m of
          Just (Cell _ (Form expr)) -> expr `shouldBe` RangeOp AvgR ("C", 1) ("D", 5)
          _ -> expectationFailure "Expected AVG expression"

  it "handles line comments" $ do
    let sheetStr = unlines
          [ "sheet {"
          , "  -- this is a comment"
          , "  A1 = 5; // another comment"
          , "}"
          ]
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right (Sheet m) -> Map.lookup ("A", 1) m `shouldBe` Just (Cell ("A", 1) (Lit (NumV 5)))

  it "reports syntax errors with useful location information" $ do
    let sheetStr = unlines
          [ "sheet {"
          , "  A1 = 10 + ;"
          , "}"
          ]
    case parseSheet sheetStr of
      Right _ -> expectationFailure "Expected parse failure"
      Left err -> do
        err `shouldContain` "2:13"
        err `shouldContain` "unexpected"
