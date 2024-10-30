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

-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input =
  case parseHelp input of
    Left err -> Left err
    Right (query, _) -> Right query

parseHelp :: String -> Either String (Query, String)
parseHelp [] = Left "Unexpected end of input"
parseHelp (' ':xs) = parseHelp xs
parseHelp ('\n':xs) = parseHelp xs
parseHelp input =
  case parseLocationQuery input of
    Just location ->
      case parseNightQuery input of
        Just nights ->
          Right (HotelStayQuery location nights, rest)
        Nothing -> parseRouteQuery input
    Nothing -> parseRouteQuery

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
  case readMaybe (takeWhile isDigit input) of
    Just n -> if n >= 1 && n <= 7
      then Just n
      else Nothing
    Nothing -> Nothing

parseHotelStayQuery :: String -> Either String (Query, String)
parseHotelStayQuery input =
  case parseLocationQuery input of
    Just location ->
      case span (/= 'f') input of
        (before, rest) ->
          case span isDigit rest of
            (nightsStr, rest') ->
              case readMaybe nightsStr of
                Just n -> if n >= 1 && n <= 7
                          then Right (HotelStayQuery location n, rest')
                          else Left "Invalid number of nights (1-7)"
                Nothing -> Left "Invalid number of nights"
            _ -> Left "Expected number of nights after 'for'"
    _ -> Left "Invalid location"

parseRouteQuery :: String -> Either String (Query, String)
parseRouteQuery input =
  case span (/= 'r') input of
    (before, "route:" ++ rest) ->
      case parseManyQueries rest of
        Right (queries, rest') -> Right (RouteQuery queries, rest')
        Left err -> Left err
    _ -> Left "Expected 'route:'"

parseManyQueries :: String -> ([Query], String)
parseManyQueries [] = ([], [])
parseManyQueries input =
  case parseHelp input of
    Left err -> Left err
    Right (query, rest) ->
      case parseManyQueries rest of
        Right (queries, rest') -> Right (query : queries, rest')
        Left err -> Left err

data State = State
  { counter :: Int
  , message :: String
  , location :: Maybe String
  , nights :: Maybe Int
  , hotelStay :: Maybe (String, Int)
  , route :: Maybe [Query]
  } deriving Show

emptyState :: State
emptyState = State 0 "" Nothing Nothing Nothing Nothing

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query =
  case query of
    LocationQuery location -> Right (Just "Location set.", State (counter state) (message state) (Just location) (nights state) (hotelStay state) (route state))
    NightQuery nights -> Right (Just "Nights set.", State (counter state) (message state) (location state) (Just nights) (hotelStay state) (route state))
    HotelStayQuery (location', nights') ->
      Right (Just "Hotel stay set.", State (counter state) (message state) (location state) (nights state) (Just (location', nights')) (route state))
    RouteQuery route ->
      Right (Just "Route set.", State (counter state) (message state) (location state) (nights state) (hotelStay state) (Just route))
    _ -> Left "Unknown query"