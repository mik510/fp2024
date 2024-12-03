{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query =
  LocationQuery String
  | NightQuery Int
  | HotelStayQuery (String, Int)
  | RouteQuery [Query]

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show (LocationQuery location) = "Location: " ++ location
  show (NightQuery nights) = "Nights: " ++ show nights
  show (HotelStayQuery (location, nights)) = "Hotel stay: " ++ location ++ " for " ++ show nights
  show (RouteQuery route) = "Route: " ++ show (showHotelStays route)

showHotelStays :: [Query] -> String
showHotelStays [] = ""
showHotelStays (HotelStayQuery (location, nights) : rest) = "Hotel stay: " ++ location ++ " for " ++ show nights ++ ", " ++ showHotelStays rest
showHotelStays (_ : rest) = showHotelStays rest

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Some error message"

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"
