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
  [ testCase "Valid Hotel Stay" $
      Lib2.parseQuery "Vilnius, Lithuania 3" @?= Right (Lib2.HotelStayQuery ("Vilnius", 3))
  , testCase "Invalid Input" $
      Lib2.parseQuery "gibberish" @?= Left "Please provide a valid input"
  , testCase "Missing Location" $
      Lib2.parseQuery "6" @?= Left "Please provide a location"
  , testCase "Missing Nights" $
      Lib2.parseQuery "Berlin, Germany" @?= Left "Please provide the number of nights"
  , testCase "Invalid Night Count" $
      Lib2.parseQuery "Warsaw, Poland 12" @?= Left "Number of nights must be between 1 and 7"
  , testCase "No input" $
      Lib2.parseQuery "       " @?= Left "Please provide an input"
  , testCase "Valid State Transition: Location" $
      Lib2.stateTransition Lib2.emptyState (Lib2.LocationQuery "Vilnius") @?= Right (Lib2.LocationState "Vilnius", ["New state: Location - Vilnius"])
  , testCase "Valid State Transition: Hotel Stay" $
      Lib2.stateTransition (Lib2.LocationState "Vilnius") (Lib2.NightQuery 3) @?= Right (Lib2.HotelStayState "Vilnius" 3, ["New state: Hotel Stay", "  Location: Vilnius", "  Nights: 3"])
  , testCase "Invalid State Transition" $
      Lib2.stateTransition Lib2.emptyState (Lib2.RouteQuery []) @?= Left "Invalid state transition"
  ]

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]