{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "Valid Hotel Stay" $
      Lib2.parseQuery "Vilnius, Lithuania 3" @?= Just (Lib2.HotelStayQuery ("Vilnius", 3))
  , testCase "Invalid Input" $
      Lib2.parseQuery "invalid input" @?= Nothing
  , testCase "Missing Location" $
      Lib2.parseQuery "3" @?= Nothing
  , testCase "Missing Nights" $
      Lib2.parseQuery "Vilnius, Lithuania" @?= Nothing
  , testCase "Invalid Night Count" $
      Lib2.parseQuery "Vilnius, Lithuania 10" @?= Nothing
  , testCase "Valid State Transition: Location" $
      Lib2.stateTransition Lib2.emptyState (Lib2.LocationQuery "Vilnius") @?= Right (Lib2.LocationState "Vilnius", ["New state: Location - Vilnius"])
  , testCase "Valid State Transition: Hotel Stay" $
      Lib2.stateTransition (Lib2.LocationState "Vilnius") (Lib2.NightQuery 3) @?= Right (Lib2.HotelStayState "Vilnius" 3, ["New state: Hotel Stay", "  Location: Vilnius", "  Nights: 3"])
  , testCase "Invalid State Transition" $
      Lib2.stateTransition Lib2.emptyState (Lib2.RouteQuery []) @?= Left "Invalid state transition"
  ]