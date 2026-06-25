module E2ESpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Lib

spec :: Spec
spec = describe "End-to-End Correctness" $ do
  it "evaluates a small hand-computed sheet correctly" $ do
    let sheetStr = unlines
          [ "sheet {"
          , "  A1 = 10;"
          , "  A2 = 20;"
          , "  A3 = A1 + A2;"
          , "  A4 = SUM(A1:A3);"
          , "}"
          ]
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (NumV 10.0)
        Map.lookup ("A", 2) env `shouldBe` Just (NumV 20.0)
        Map.lookup ("A", 3) env `shouldBe` Just (NumV 30.0)
        Map.lookup ("A", 4) env `shouldBe` Just (NumV 60.0)

  it "evaluates a sheet containing cycles and independent acyclic cells without hanging" $ do
    let sheetStr = unlines
          [ "sheet {"
          , "  A1 = A2;"
          , "  A2 = A1;"
          , "  B1 = 100;"
          , "  B2 = B1 + 50;"
          , "}"
          ]
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (ErrV "cycle")
        Map.lookup ("A", 2) env `shouldBe` Just (ErrV "cycle")
        Map.lookup ("B", 1) env `shouldBe` Just (NumV 100.0)
        Map.lookup ("B", 2) env `shouldBe` Just (NumV 150.0)

  describe "Reactive Recalculation E2E" $ do
    it "reactively recalculates only downstream dependents" $ do
      let sheetStr = unlines
            [ "sheet {"
            , "  A1 = 10;"
            , "  A2 = 20;"
            , "  A3 = A1 + A2;"
            , "  A4 = SUM(A1:A3);"
            , "}"
            ]
      case parseSheet sheetStr of
        Left err -> expectationFailure err
        Right sheet -> do
          let env = evalSheet sheet
          let (_, newEnv) = updateCell ("A", 2) (Lit (NumV 50)) sheet env
          Map.lookup ("A", 1) newEnv `shouldBe` Just (NumV 10)
          Map.lookup ("A", 2) newEnv `shouldBe` Just (NumV 50)
          Map.lookup ("A", 3) newEnv `shouldBe` Just (NumV 60)
          Map.lookup ("A", 4) newEnv `shouldBe` Just (NumV 120)

    it "breaks cycles and updates values upon cycle-breaking update" $ do
      let sheetStr = unlines
            [ "sheet {"
            , "  A1 = A2;"
            , "  A2 = A1;"
            , "  B1 = 100;"
            , "  B2 = B1 + 50;"
            , "}"
            ]
      case parseSheet sheetStr of
        Left err -> expectationFailure err
        Right sheet -> do
          let env = evalSheet sheet
          let (_, newEnv) = updateCell ("A", 2) (Lit (NumV 50)) sheet env
          Map.lookup ("A", 2) newEnv `shouldBe` Just (NumV 50)
          Map.lookup ("A", 1) newEnv `shouldBe` Just (NumV 50)
          Map.lookup ("B", 1) newEnv `shouldBe` Just (NumV 100)
          Map.lookup ("B", 2) newEnv `shouldBe` Just (NumV 150)

  it "evaluates the operators.sheet example file correctly" $ do
    sheetStr <- readFile "spreadsheets/operators.sheet"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (NumV 5.0)
        Map.lookup ("A", 2) env `shouldBe` Just (NumV 2.0)
        Map.lookup ("A", 3) env `shouldBe` Just (NumV 7.0)
        Map.lookup ("A", 4) env `shouldBe` Just (NumV 3.0)
        Map.lookup ("A", 5) env `shouldBe` Just (NumV 10.0)
        Map.lookup ("A", 6) env `shouldBe` Just (NumV 2.5)
        Map.lookup ("A", 7) env `shouldBe` Just (NumV 25.0)
        Map.lookup ("A", 8) env `shouldBe` Just (BoolV True)
        Map.lookup ("A", 9) env `shouldBe` Just (BoolV False)
        Map.lookup ("B", 1) env `shouldBe` Just (BoolV True)
        Map.lookup ("B", 2) env `shouldBe` Just (NumV 47.5)
        Map.lookup ("B", 3) env `shouldBe` Just (NumV 9.5)
        Map.lookup ("B", 4) env `shouldBe` Just (NumV 2.5)
        Map.lookup ("B", 5) env `shouldBe` Just (NumV 25.0)
        Map.lookup ("B", 6) env `shouldBe` Just (NumV 5.0)

  it "evaluates the errors.sheet example file correctly" $ do
    sheetStr <- readFile "spreadsheets/errors.sheet"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (ErrV "#DIV/0!")
        Map.lookup ("A", 2) env `shouldBe` Just (ErrV "#VALUE!")
        Map.lookup ("A", 3) env `shouldBe` Just (ErrV "#VALUE!")
        Map.lookup ("A", 4) env `shouldBe` Just (ErrV "Missing reference: (\"Z\",99)")
        Map.lookup ("A", 5) env `shouldBe` Just (ErrV "#DIV/0!")
        Map.lookup ("A", 6) env `shouldBe` Just (ErrV "#VALUE!")
        Map.lookup ("A", 7) env `shouldBe` Just (ErrV "Missing reference: (\"Z\",99)")
        Map.lookup ("B", 1) env `shouldBe` Just (NumV 100.0)
        Map.lookup ("B", 2) env `shouldBe` Just (NumV 200.0)
        Map.lookup ("B", 3) env `shouldBe` Just (ErrV "#DIV/0!")

  it "evaluates the datatypes.sheet example file correctly" $ do
    sheetStr <- readFile "spreadsheets/datatypes.sheet"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (NumV 3.14159)
        Map.lookup ("A", 2) env `shouldBe` Just (NumV (-42.0))
        Map.lookup ("B", 1) env `shouldBe` Just (BoolV True)
        Map.lookup ("B", 2) env `shouldBe` Just (BoolV False)
        Map.lookup ("C", 1) env `shouldBe` Just (StrV "hello")
        Map.lookup ("C", 2) env `shouldBe` Just (StrV "world")
        Map.lookup ("C", 3) env `shouldBe` Just (StrV "hello")
        Map.lookup ("D", 1) env `shouldBe` Just (BoolV True)
        Map.lookup ("D", 2) env `shouldBe` Just (BoolV False)
        Map.lookup ("D", 3) env `shouldBe` Just (BoolV True)
        Map.lookup ("D", 4) env `shouldBe` Just (BoolV False)
        Map.lookup ("D", 5) env `shouldBe` Just (BoolV True)
        Map.lookup ("E", 1) env `shouldBe` Just (BoolV False)
        Map.lookup ("E", 2) env `shouldBe` Just (BoolV False)
        Map.lookup ("E", 3) env `shouldBe` Just (ErrV "#VALUE!")
        Map.lookup ("E", 4) env `shouldBe` Just (ErrV "#VALUE!")

  it "evaluates the complex_deps.sheet example file correctly" $ do
    sheetStr <- readFile "spreadsheets/complex_deps.sheet"
    case parseSheet sheetStr of
      Left err -> expectationFailure err
      Right sheet -> do
        let env = evalSheet sheet
        Map.lookup ("A", 1) env `shouldBe` Just (NumV 10.0)
        Map.lookup ("A", 2) env `shouldBe` Just (NumV 20.0)
        Map.lookup ("A", 3) env `shouldBe` Just (NumV 30.0)
        Map.lookup ("A", 4) env `shouldBe` Just (NumV 40.0)
        Map.lookup ("B", 1) env `shouldBe` Just (NumV 30.0)
        Map.lookup ("B", 2) env `shouldBe` Just (NumV 50.0)
        Map.lookup ("B", 3) env `shouldBe` Just (NumV 70.0)
        Map.lookup ("C", 1) env `shouldBe` Just (NumV 150.0)
        Map.lookup ("C", 2) env `shouldBe` Just (NumV 40.0)
        Map.lookup ("D", 1) env `shouldBe` Just (NumV 300.0)
        Map.lookup ("D", 2) env `shouldBe` Just (NumV 260.0)
        Map.lookup ("E", 1) env `shouldBe` Just (NumV 10.0)
        Map.lookup ("E", 2) env `shouldBe` Just (NumV 300.0)
        Map.lookup ("E", 3) env `shouldBe` Just (NumV 8.0)
