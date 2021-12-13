{-# LANGUAGE OverloadedStrings #-}
module Parse
    ( parse
    , parse_graph
    , read_file
    ) where

import Insert 
import DataTypes

[-- breaks each line into individual elements
breakLine :: String 
          -> String 
          -> [String] 
          -> [String]
breakLine [] y ys = (reverse y):ys
breakLine (',':xs) y ys = breakLine xs "" ((reverse y):ys)
breakLine (c:xs) y ys = breakLine xs (c:y) ys

[-- csv -> string
read_file :: String 
          -> IO String
read_file crime = readFile $ crime ++ ".csv"

[-- Parse all the lines in the document
parse :: String -> String -> [CrimetoCompare]
parse crime s = map (convert crime) (tail $ lines s)

[-- Returns user selected data for graph
convert_graph :: String -> String -> String -> (String,[Double])
convert_graph currency g s
 | g == "Total" = (d, [read o])
 | g == "Type" = (d, [read c])
 | g == "Percentage" = (d, [read l])
 | g == "Month" = (d, [read a])
 where [d,o,h,l,c,a,v] = reverse $ breakLine s "" []

[-- Parses through lines to get user selected data for graph
parse_graph :: String -> String -> String -> [(String,[Double])]
parse_graph crime g s = map (convert_graph crime g) (tail $ lines s)
