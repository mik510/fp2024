{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

data Query =
  LocationQuery String
  | NightQuery Int
  | HotelStayQuery (String, Int)
  | RouteQuery [Query]

instance Eq Query where
  (==) :: Query -> Query -> Bool
  (==) (LocationQuery l1) (LocationQuery l2) = l1 == l2
  (==) (NightQuery n1) (NightQuery n2) = n1 == n2
  (==) (HotelStayQuery (l1, n1)) (HotelStayQuery (l2, n2)) = l1 == l2 && n1 == n2
  (==) _ _ = False

instance Show Query where
  show :: Query -> String
  show (LocationQuery location) = "Location: " ++ location
  show (NightQuery nights) = "Nights: " ++ show nights
  show (HotelStayQuery (location, nights)) = "Hotel stay: " ++ location ++ " for " ++ show nights
  show (RouteQuery route) = "Route: " ++ show (showHotelStays route)

showHotelStays :: [Query] -> String
showHotelStays [] = ""
showHotelStays (HotelStayQuery (location, nights) : rest) = "Hotel stay: " ++ location ++ " for " ++ show nights ++ ", " ++ showHotelStays rest
showHotelStays (_ : rest) = showHotelStays rest

parseQuery :: String -> Maybe Query
parseQuery input =
  case read input :: Maybe (String, Int) of
    Just (location, nights)
      | 1 <= nights && nights <= 7 -> Just $ HotelStayQuery (location, nights)
      | otherwise -> Nothing
    Nothing ->
      case parseLocationQuery input of
        Just location -> Just $ LocationQuery location
        Nothing ->
          case parseNightQuery input of
            Just nights -> Just $ NightQuery nights
            Nothing -> Nothing

parseLocationQuery :: String -> Maybe String
parseLocationQuery input =
  case input of
    "Vilnius, Lithuania" -> Just "Vilnius"
    "Warsaw, Poland" -> Just "Warsaw"
    "Prague, Czechia" -> Just "Prague"
    "Vienna, Austria" -> Just "Vienna"
    "Berlin, Germany" -> Just "Berlin"
    _ -> Nothing

parseNightQuery :: String -> Maybe Int
parseNightQuery input =
  case read input :: Maybe Int of
    Just n | 1 <= n && n <= 7 -> Just n
    _ -> Nothing

data State = 
  EmptyState
  | LocationState String
  | NightsState String Int
  | HotelStayState String Int
  | RouteState [Query]

emptyState :: State
emptyState = EmptyState

stateTransition :: State -> Query -> Maybe State
stateTransition EmptyState (LocationQuery location) = Just $ LocationState location
stateTransition (LocationState location) (NightQuery nights) = Just $ HotelStayState location nights
stateTransition _ _ = Just EmptyState