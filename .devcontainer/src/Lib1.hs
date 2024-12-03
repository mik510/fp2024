module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    "completions",
    "locations",
    "nights",
    "hotelStays",
    "routes",
    "route"
]

locations :: [String]
locations = [
    "Vilnius, Lithuania", 
    "Warsaw, Poland", 
    "Prague, Czechia", 
    "Vienna, Austria", 
    "Berlin, Germany"
]

nights :: [Int]
nights = [1..7]

hotelStays :: [(String, Int)]
hotelStays = [(location, nights) | location <- locations, nights <- nights]

routes :: [[(String, Int)]]
routes = route locations where
    route :: [String] -> [[(String, Int)]]
    route [] = [[]]
    route (locaton: rest) = do
        hotelStay <- hotelStays
        restRoutes <- route rest
        return (hotelStay : restRoutes)
