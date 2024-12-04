{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1: Valid Hotel Stay" $
      Lib2.parseQuery "Vilnius, Lithuania 3" @?= Right (Lib2.HotelStayQuery ("Vilnius", 3))
    testCase "Parsing case 2: Invalid input" $
      Lib2.parseQuery "o" @?= Left "Invalid input"
    testCase "Parsing case 3: Missing Location" $
      Lib2.parseQuery "2" @?= Left "No location provided"
    testCase "Parsing case 4: Missing Nights" $
      Lib2.parseQuery "Berlin, Germany" @?= Left "No nights provided"
  ]

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]