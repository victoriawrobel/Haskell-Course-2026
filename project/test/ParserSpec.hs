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

  it "respects operator precedence (+ vs * vs ^ vs ==)" $ do
    let sheetStr = "sheet { A1 = 10 + 2 * 3 ^ 2 == 28; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right (Sheet m) -> do
        case Map.lookup ("A", 1) m of
          Just (Cell _ (Form expr)) ->
            expr `shouldBe` BinOp EqOp
              (BinOp Add
                (LitE (NumV 10))
                (BinOp Mul
                  (LitE (NumV 2))
                  (BinOp Pow (LitE (NumV 3)) (LitE (NumV 2)))))
              (LitE (NumV 28))
          _ -> expectationFailure "Expected A1 to be a Formula"

  it "parses all binary operators (+, -, *, /, ^, ==, <, >)" $ do
    let sheetStr = "sheet { A1 = 1 + 2 - 3 * 4 / 5 ^ 6 == 7; A2 = 1 < 2; A3 = 2 > 1; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right (Sheet m) -> do
        Map.member ("A", 1) m `shouldBe` True
        Map.member ("A", 2) m `shouldBe` True
        Map.member ("A", 3) m `shouldBe` True

  it "parses range expressions (SUM, AVG, MIN, MAX, COUNT)" $ do
    let sheetStr = "sheet { A1 = SUM(B1:B10); A2 = AVG(C1:D5); A3 = MIN(E1:E2); A4 = MAX(F1:F2); A5 = COUNT(G1:G10); }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right (Sheet m) -> do
        case Map.lookup ("A", 1) m of
          Just (Cell _ (Form expr)) -> expr `shouldBe` RangeOp SumR ("B", 1) ("B", 10)
          _ -> expectationFailure "Expected SUM expression"
        case Map.lookup ("A", 2) m of
          Just (Cell _ (Form expr)) -> expr `shouldBe` RangeOp AvgR ("C", 1) ("D", 5)
          _ -> expectationFailure "Expected AVG expression"
        case Map.lookup ("A", 3) m of
          Just (Cell _ (Form expr)) -> expr `shouldBe` RangeOp MinR ("E", 1) ("E", 2)
          _ -> expectationFailure "Expected MIN expression"
        case Map.lookup ("A", 4) m of
          Just (Cell _ (Form expr)) -> expr `shouldBe` RangeOp MaxR ("F", 1) ("F", 2)
          _ -> expectationFailure "Expected MAX expression"
        case Map.lookup ("A", 5) m of
          Just (Cell _ (Form expr)) -> expr `shouldBe` RangeOp CountR ("G", 1) ("G", 10)
          _ -> expectationFailure "Expected COUNT expression"

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
