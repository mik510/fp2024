{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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

data Location = Vilnius | Warsaw | Prague | Vienna | Berlin deriving (Eq, Show)
data Nights = Nights Int deriving (Eq, Show)
data HotelStay = HotelStay Location Nights deriving (Show)
data Route = SingleStay HotelStay | MultipleStays HotelStay Route deriving (Show)

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

parseQuery :: String -> Either String Query
parseQuery input =
  case read input :: Maybe (String, Int) of
    Just (location, nights)
      | 1 <= nights && nights <= 7 -> Right $ HotelStayQuery (location, nights)
      | otherwise -> Left "Invalid number of nights"
    Nothing ->
      case parseLocationQuery input of
        Just location -> Right $ LocationQuery (show location)
        Nothing ->
          case parseNightQuery input of
            Just (Nights n) -> Right $ NightQuery n
            Nothing ->
              case parseRouteQuery input of
                Just route -> Right $ RouteQuery (routeToQueries route)
                Nothing -> Left "Invalid input format"

-- <locations> ::= "Vilnius, Lithuania" | "Warsaw, Poland" | "Prague, Czechia" | "Vienna, Austria" | "Berlin, Germany"
parseLocationQuery :: String -> Maybe Location
parseLocationQuery input =
  case input of
    "Vilnius, Lithuania" -> Just Vilnius
    "Warsaw, Poland" -> Just Warsaw
    "Prague, Czechia" -> Just Prague
    "Vienna, Austria" -> Just Vienna
    "Berlin, Germany" -> Just Berlin
    _ -> Nothing

-- <nights> ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 
parseNightQuery :: String -> Maybe Nights
parseNightQuery input =
  case read input :: Maybe Int of
    Just n | 1 <= n && n <= 7 -> Just (Nights n)
    _ -> Nothing

-- <hotelStays> ::= <locations><nights>
parseHotelStayQuery :: String -> Maybe HotelStay
parseHotelStayQuery input =
  case parseLocationQuery input of
    Just location -> 
      case parseNightQuery (drop (length (show location) + 2) input) of
        Just nights -> Just (HotelStay location nights)
        Nothing -> Nothing
    Nothing -> Nothing

-- <routes> ::= <hotelStays> | <hotelStays><routes>
parseRouteQuery :: String -> Maybe Route
parseRouteQuery [] = Nothing
parseRouteQuery input =
  case parseHotelStayQuery input of
    Just stay -> 
      case parseRouteQuery (drop (length (show stay)) input) of
        Nothing -> Just (SingleStay stay)
        Just route -> Just (MultipleStays stay route)
    Nothing -> Nothing

-- parse helper (routes to queries)
routeToQueries :: Route -> [Query]
routeToQueries (SingleStay (HotelStay location (Nights nights))) = 
  [HotelStayQuery (show location, nights)]
routeToQueries (MultipleStays (HotelStay location (Nights nights)) route) = 
  HotelStayQuery (show location, nights) : routeToQueries route

data State = 
  EmptyState
  | LocationState String
  | NightsState String Int
  | HotelStayState String Int
  | RouteState [Query]
  | ErrorState String
  deriving (Show)

emptyState :: State
emptyState = EmptyState

stateTransition :: State -> Query -> Either String ([String], State)
stateTransition EmptyState (LocationQuery location) = 
  Right (["Location set to: " ++ location], LocationState location)
stateTransition (LocationState location) (NightQuery nights) = 
  Right (["Nights set to: " ++ show nights], HotelStayState location nights)
stateTransition (HotelStayState location nights) (RouteQuery route) = 
  Right (["Route created for: " ++ location ++ " with " ++ show nights ++ " nights"], RouteState route)
stateTransition _ _ = 
  Left "Invalid state transition"