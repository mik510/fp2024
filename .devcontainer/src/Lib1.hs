module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = []

location :: [String]
location = ["Vilnius, Lithuania", "Warsaw, Poland", "Prague, Czechia", "Vienna, Austria", "Berlin, Germany"]

nights :: [Int]
nights = [1..7]

hotelStay :: [(String, Int)]
hotelStay = [(location, nights) | location <- location, nights <- nights]

route :: [String] -> [[(String, Int)]]
route = [] = [[]]
--route 