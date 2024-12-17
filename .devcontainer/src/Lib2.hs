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
data Route = SingleStay HotelStay 
  | MultipleStays HotelStay Route deriving (Show)
 
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
  show (RouteQuery route) = "Single hotel stay route: " ++ show (SingleStay)
--  show (RouteQuery route) = "Multiple hotel stays route: " ++ show (MultipleStays)

type Parser a = String -> Either String (a, String)

-- <root> ::= <command> 
parseQuery :: String -> Either String Query 
parseQuery input = 
  case parseCommandQuery input of 
    Right (Add, rest) -> 
      case parseLocation rest of 
        Left err -> Left err 
        Right (location, rest') -> 
          case parseNights rest' of 
          Left err -> Left err 
          Right (nights, _) -> Right (RouteQuery [HotelStayQuery (location, nights)]) 
    Right (Remove, _) -> Left "Remove not yet implemented" 
    Right (Change, _) -> Left "Change not yet implemented" 
    Left err -> Left err

-- <command> ::= <addHotelStay> | <removeHotelStay> | <changeHotelStay>
parseCommandQuery :: String -> Either String (Command, String) 
parseCommandQuery input = 
  case input of 
    "add" -> Right (Add, drop (length "add") input) 
    "remove" -> Right (Remove, drop (length "remove") input) 
    "change" -> Right (Change, drop (length "change") input) 
    _ -> Left "Invalid command" 

-- <addHotelStay> ::= add <hotelStays>
parseAddHotelStayQuery :: String -> Either String (Command, HotelStay)
parseAddHotelStayQuery input =
  case parseCommandQuery input of
    Right (Add, rest) ->
      case parseHotelStayQuery rest of
        Left err -> Left err
        Right hotelStay -> Right (Add, hotelStay)
    _ -> Left "Invalid command"

-- TODO implement remove command
-- <removeHotelStay> ::= remove <hotelStays> 
parseRemoveHotelStayQuery :: String -> Either String (Command, HotelStay)
parseRemoveHotelStayQuery input =
  case parseCommandQuery input of
    Right (Remove, rest) ->
      case parseHotelStayQuery rest of
        Left err -> Left err
        Right hotelStay -> Right (Remove, hotelStay)
    _ -> Left "Invalid command"

--TODO implement change command; use remove, then add
-- <changeHotelStay> ::= change <hotelStays><hotelStays> 
parseChangeHotelStayQuery :: String -> Either String (Command, HotelStay, HotelStay)
parseChangeHotelStayQuery input =
  case parseCommandQuery input of
    Right (Change, rest) ->
      case parseHotelStayQuery rest of
        Left err -> Left err
        Right hotelStay -> Right (Change, hotelStay, hotelStay)
    _ -> Left "Invalid command"

-- <hotelStays> ::= <locations><nights>
parseHotelStayQuery :: String -> Either String HotelStay
parseHotelStayQuery input =
  case parseLocationQuery input of
    Right location ->
      case parseNightsQuery input of
        Right nights -> Right (HotelStay location nights)
        _ -> Left "Invalid nights"
    _ -> Left "Invalid location"

-- <locations> ::= "Vilnius, Lithuania" | "Warsaw, Poland" | "Prague, Czechia" 
-- | "Vienna, Austria" | "Berlin, Germany"
parseLocationQuery :: String -> Either String Location
parseLocationQuery input =
  case input of
    "Vilnius, Lithuania" -> Right Vilnius
    "Warsaw, Poland" -> Right Warsaw
    "Prague, Czechia" -> Right Prague
    "Vienna, Austria" -> Right Vienna
    "Berlin, Germany" -> Right Berlin
    _ -> Left "Invalid location"

-- <nights> ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 
parseNightsQuery :: String -> Either String Nights
parseNightsQuery input =
  case read input :: Maybe Int of
    Just n | 1 <= n && n <= 7 -> Right (Nights n)
    _ -> Left "Invalid nights"

-- <routes> ::= <hotelStays> | <hotelStays><routes>
parseRouteQuery :: String -> Either String Route
parseRouteQuery input =
  case parseHotelStayQuery input of
    Right stay ->
      case parseRouteQuery input of
        _ -> Right (SingleStay stay)
        route -> Right (MultipleStays stay route)
    _ -> Left "Something went wrong"

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