module EvalSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Lib

spec :: Spec
spec = describe "Evaluator Correctness" $ do
  it "evaluates individual arithmetic operations and comparisons" $ do
    let sheetStr = unlines
          [ "sheet {"
          , "  A1 = 10 + 5;"
          , "  A2 = 10 - 5;"
          , "  A3 = 10 * 5;"
          , "  A4 = 10 / 5;"
          , "  A5 = 2 ^ 3;"
          , "  A6 = 5 == 5;"
          , "  A7 = 5 == 6;"
          , "  A8 = 5 < 6;"
          , "  A9 = 5 > 6;"
          , "  B1 = MIN(A1:A4);"
          , "  B2 = MAX(A1:A4);"
          , "  B3 = COUNT(A1:A4);"
          , "  B4 = SUM(A1:A4);"
          , "  B5 = AVG(A1:A4);"
          , "}"
          ]
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (NumV 15.0)
        Map.lookup ("A", 2) env `shouldBe` Just (NumV 5.0)
        Map.lookup ("A", 3) env `shouldBe` Just (NumV 50.0)
        Map.lookup ("A", 4) env `shouldBe` Just (NumV 2.0)
        Map.lookup ("A", 5) env `shouldBe` Just (NumV 8.0)
        Map.lookup ("A", 6) env `shouldBe` Just (BoolV True)
        Map.lookup ("A", 7) env `shouldBe` Just (BoolV False)
        Map.lookup ("A", 8) env `shouldBe` Just (BoolV True)
        Map.lookup ("A", 9) env `shouldBe` Just (BoolV False)
        Map.lookup ("B", 1) env `shouldBe` Just (NumV 2.0)
        Map.lookup ("B", 2) env `shouldBe` Just (NumV 50.0)
        Map.lookup ("B", 3) env `shouldBe` Just (NumV 4.0)
        Map.lookup ("B", 4) env `shouldBe` Just (NumV 72.0)
        Map.lookup ("B", 5) env `shouldBe` Just (NumV 18.0)

  it "evaluates division by zero safely (returns #DIV/0!)" $ do
    let sheetStr = "sheet { A1 = 10 / 0; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (ErrV "#DIV/0!")

  it "evaluates type mismatches safely (returns #VALUE!)" $ do
    let sheetStr = "sheet { A1 = TRUE + 1; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (ErrV "#VALUE!")

  it "detects cycles on a tiny A1 = A1 sheet" $ do
    let sheetStr = "sheet { A1 = A1; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (ErrV "cycle")

  it "handles larger and multiple cycle groups" $ do
    let sheetStr = unlines
          [ "sheet {"
          , "  A1 = A2;"
          , "  A2 = A3;"
          , "  A3 = A1;"
          , "  B1 = B2;"
          , "  B2 = B1;"
          , "}"
          ]
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (ErrV "cycle")
        Map.lookup ("A", 2) env `shouldBe` Just (ErrV "cycle")
        Map.lookup ("A", 3) env `shouldBe` Just (ErrV "cycle")
        Map.lookup ("B", 1) env `shouldBe` Just (ErrV "cycle")
        Map.lookup ("B", 2) env `shouldBe` Just (ErrV "cycle")

  it "computes correct topological order for an acyclic sheet" $ do
    let sheetStr = unlines
          [ "sheet {"
          , "  A3 = A2 * 2;"
          , "  A1 = 10;"
          , "  A2 = A1 + 5;"
          , "}"
          ]
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let (acyclicOrder, cycles) = findCyclesAndOrder sheet
        cycles `shouldBe` []
        acyclicOrder `shouldBe` [("A", 1), ("A", 2), ("A", 3)]

