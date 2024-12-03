{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L

-- <root> ::= <commands>
-- <command> ::= <addHotelStay> | <removeHotelStay> | <changeHotelStay>
-- <addHotelStay> ::= add <hotelStays>
-- <removeHotelStay> ::= remove <hotelStays>
-- <changeHotelStay> ::= change <hotelStays><hotelStays>

-- <locations> ::= "Vilnius, Lithuania" | "Warsaw, Poland" | "Prague, Czechia" | "Vienna, Austria" | "Berlin, Germany"
-- <nights> ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 
-- <hotelStays> ::= <locations><nights>
-- <routes> ::= <hotelStays> | <hotelStays><routes>

data Query =
  LocationQuery String
  | NightQuery Int
  | HotelStayQuery (String, Int)
  | RouteQuery [Query]

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

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input =
  case read input :: Maybe (String, Int) of
    Just(location, nights)
      | 1 <= nights && nights <= 7 -> Right $ HotelStayQuery (location, nights)
      | otherwise -> Left "Invalid number of nights"
    Nothing ->
      case parseLocationQuery input of
        Just location -> Right $ LocationQuery (show location)
        Nothing ->
          case parseNightsQuery input of
            Just (Nights n) -> Right $ NightQuery n
            Nothing ->
              case parseRouteQuery input of
                Just route -> Right $ RouteQuery (rotateToQueries route)
                Nothing -> Left "Invalid input format"

-- TODO implement these:
parseLocationQuery :: String -> Maybe Location
parseNightsQuery :: String -> Maybe Nights
parseHotelStayQuery :: String -> Maybe HotelStay
parseRouteQuery :: String -> Maybe Route


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