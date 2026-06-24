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
