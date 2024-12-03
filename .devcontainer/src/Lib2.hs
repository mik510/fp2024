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

-- TODO implement main parser
-- <root> ::= <command>
parseQuery :: String -> Either String Query
parseQuery input =
  case read input :: Maybe (String, Int) of
      

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
    Add -> 
      case RouteQuery input of
        Just (location nights) -> Just (HotelStay (location, nights))
        _ -> "Invalid hotel stay to add"
    _ -> "Invalid command"

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


type Parser a = String -> Either String (a, String)

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

{-
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)
-}