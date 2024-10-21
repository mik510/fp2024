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
import Text.Parsec (Parser, parse)
-- ^ removed State(stateInput) from brackets because of ambiguity

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
  case parse (queryParser :: Parser Query) "" input of
    Left err -> Left $ show err
    Right query -> Right query

queryParser :: Parser Query
queryParser =
  LocationQuery <$> string "location:" <*> many (noneOf " \n")
  <|> NightQuery <$> string "nights:" <*> many (noneOf " \n")
  <|> HotelStayQuery <$> string "location:" <*> many (noneOf " \n") <*> string "for" <*> many (noneOf "\n")
  <|> RouteQuery <$> string "route:" <*> many (queryParser)

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