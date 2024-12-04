{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L

data Query =
  CommandQuery String
  | LocationQuery String
  | NightQuery Int
  | HotelStayQuery (String, Int)
  | RouteQuery [Query]

data Command = Add | Remove | Change deriving (Eq, Show)
data Location = Vilnius | Warsaw | Prague 
  | Vienna | Berlin deriving (Eq, Show)
data Nights = Nights Int deriving (Eq, Show)
data HotelStay = HotelStay Location Nights deriving (Show)
data Route = SingleStay HotelStay HotelStay | MultipleStays HotelStay Route deriving (Show)
 
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

type Parser a = String -> Either String (a, String)

-- TODO implement main parser
-- <root> ::= <command>
parseQuery :: String -> Either String Query
parseQuery input =
  case parseCommandQuery input of

-- <command> ::= <addHotelStay> | <removeHotelStay> | <changeHotelStay>
parseCommandQuery :: String -> Either String Command
parseCommandQuery input =
  case input of
    "add" -> Just Add
    "remove" -> Just Remove
    "change" -> Just Change
    _ -> Left "Invalid command"

-- TODO fix this
-- <addHotelStay> ::= add <hotelStays>
parseAddHotelStayQuery :: String -> Either String (Command, HotelStay)
parseAddHotelStayQuery input =
  case parseCommandQuery input of
    () -> 
      case RouteQuery input of
        Just (location nights) -> Just (HotelStay (location, nights))
        _ -> "Invalid hotel stay to add"
    _ -> "Invalid command"

-- 
-- EG
parseAddHotelStayQuery :: String -> Either String (Command, HotelStay)
parseAddHotelStayQuery input =
  case parseAdd input of
    Left e -> Left "We fucked up"
    (_, r1) -> case parseHotelStays r1 of
      Left e -> Left "Uh oh, baaaad hotel stays!"
      (hotelStays, r2) -> Right Add hotelStays

parseAdd :: String -> Either String (Add, String)
parseAdd input = case parseLiteral "Add" input of
  Left e -> Left "Boo"
  Right (_, r) -> Right (Add, r)

parseLiteral :: String -> String -> Either String (String, String)
parseLiteral [] input = Right ("", input)
parseLiteral (x:xs) input = case parseChar x input of
  Left e -> Left "Oopsy~"
  Right (_, r) -> parseLiteral xs r

parseChar :: Char -> String -> Eiter String (Char, String)
parseChar char (x:xs) = case char == x of
  True -> Right (x, xs)
  False -> Left "Failed to parse Character!"

-- TODO implement remove command
-- <removeHotelStay> ::= remove <hotelStays> 
parseRemoveHotelStayQuery :: String -> Either String (Command, HotelStay)
parseRemoveHotelStayQuery input =
  case parseCommandQuery input of
    Remove ->
      case RouteQuery input of
        Just (location, nights) -> Just (HotelStay (location, nights))

--TODO implement change command; use remove, then add
-- <changeHotelStay> ::= change <hotelStays><hotelStays> 
parseChangeHotelStayQuery :: String -> Either String (Command, HotelStay, HotelStay)
parseChangeHotelStayQuery input =
  case parseCommandQuery input of
    Change -> parseRemoveHotelStayQuery parseAddHotelStayQuery

-- <locations> ::= "Vilnius, Lithuania" | "Warsaw, Poland" | "Prague, Czechia" 
-- | "Vienna, Austria" | "Berlin, Germany"
parseLocationQuery :: String -> Either String Location
parseLocationQuery input =
  case input of
    "Vilnius, Lithuania" -> Just Vilnius
    "Warsaw, Poland" -> Just Warsaw
    "Prague, Czechia" -> Just Prague
    "Vienna, Austria" -> Just Vienna
    "Berlin, Germany" -> Just Berlin
    _ -> Left "Invalid location"

-- <nights> ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 
parseNightsQuery :: String -> Either String Nights
parseNightsQuery input =
  case read input :: Maybe Int of
    Just n | 1 <= n && n <= 7 -> Just (Nights n)
    _ -> "Invalid nights"

-- <hotelStays> ::= <locations><nights>
parseHotelStayQuery :: String -> Either String HotelStay
parseHotelStayQuery input =
  case parseLocationQuery input of
    Just location ->
      case parseNightsQuery input of
        Just nights -> Just (HotelStay location nights)
        _ -> "Invalid nights"
    _ -> "Invalid location"

-- <routes> ::= <hotelStays> | <hotelStays><routes>
parseRouteQuery :: String -> Either String Route
parseRouteQuery input =
  case parseHotelStayQuery input of
    Just stay ->
      case parseRouteQuery input of
        Nothing -> Just (SingleStay stay)
        Just route -> Just (MultipleStays stay route)
    Nothing -> Nothing

data State =
  EmptyState
  | LocationState String
  | NightsState Int
  | HotelStayState String Int
  | RouteState [Query]
  | ErrorState String
  deriving (Show)

emptyState :: State
emptyState = EmptyState

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition EmptyState (LocationQuery location) =
  Right (["Location set to: " ++ show location], LocationState location)
stateTransition (LocationState location) (NightQuery nights) =
  Right (["Nights set to: " ++ show nights], HotelStayState location nights)
stateTransition (HotelStayState location nights) (RouteQuery route) =
  Right (["Route created for: " ++ show location ++ " for " ++ show nights ++ " nights"], RouteState route)
stateTransition _ _ =
  Left "Invalid state transition"