module PropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import Lib
import Graph (colToIndex, indexToCol)

-- Custom arbitrary generator for valid column strings (uppercase A-Z)
newtype ColString = ColString String deriving (Show)

instance Arbitrary ColString where
  arbitrary = do
    len <- choose (1, 4)
    ColString <$> vectorOf len (elements ['A'..'Z'])

spec :: Spec
spec = describe "Property-Based Invariant Tests" $ do
  describe "Column Index Conversions" $ do
    it "converts column string -> index -> column string correctly (roundtrip)" $
      property $ \(ColString s) -> indexToCol (colToIndex s) == s

    it "converts column index -> string -> index correctly (roundtrip)" $
      property $ forAll (choose (1, 500000)) $ \n -> colToIndex (indexToCol n) == n

  describe "Spreadsheet Invariants" $ do
    it "invariant: recomputation after no change yields the same values" $
      property $ do
        d1 <- arbitrary :: Gen Double
        d2 <- arbitrary :: Gen Double
        let sheetStr = unlines
              [ "sheet {"
              , "  A1 = " ++ show d1 ++ ";"
              , "  A2 = " ++ show d2 ++ ";"
              , "  A3 = A1 + A2;"
              , "  A4 = SUM(A1:A3);"
              , "}"
              ]
        return $ case parseSheet sheetStr of
          Left _ -> discard
          Right sheet ->
            let env1 = evalSheet sheet
                -- Update A2 to the EXACT same value it had
                (_, env2) = updateCell ("A", 2) (Lit (NumV d2)) sheet env1
            in env1 == env2

    it "invariant: for an acyclic sheet, a cell's value is a function of its inputs only (independent of unrelated cell updates)" $
      property $ do
        d1 <- arbitrary :: Gen Double
        d2 <- arbitrary :: Gen Double
        db1 <- arbitrary :: Gen Double
        db2 <- arbitrary `suchThat` (/= db1)
        let makeSheet db = case parseSheet $ unlines
              [ "sheet {"
              , "  A1 = " ++ show d1 ++ ";"
              , "  A2 = " ++ show d2 ++ ";"
              , "  A3 = A1 + A2;"
              , "  B1 = " ++ show db ++ ";"
              , "}"
              ] of
                Left err -> error err
                Right s -> s
        let sheet1 = makeSheet db1
            sheet2 = makeSheet db2
            env1 = evalSheet sheet1
            env2 = evalSheet sheet2
        return $ Map.lookup ("A", 3) env1 == Map.lookup ("A", 3) env2
