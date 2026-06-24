module EvalSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Lib

spec :: Spec
spec = describe "Evaluator Correctness" $ do
  it "evaluates individual arithmetic operations" $ do
    let sheetStr = "sheet { A1 = 10 + 5; A2 = 10 - 5; A3 = 10 * 5; A4 = 10 / 5; }"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (NumV 15.0)
        Map.lookup ("A", 2) env `shouldBe` Just (NumV 5.0)
        Map.lookup ("A", 3) env `shouldBe` Just (NumV 50.0)
        Map.lookup ("A", 4) env `shouldBe` Just (NumV 2.0)

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
