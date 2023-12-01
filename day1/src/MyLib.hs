module MyLib (calibValue) where

import Data.Char
import Data.List

type NumberTable = [(String, Int)]

numbers :: NumberTable
numbers = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)]

reversedNumbers :: NumberTable
reversedNumbers = map (\(s, d) -> (reverse s, d)) numbers


startsWithNumber :: NumberTable -> String -> Maybe Int
startsWithNumber nt = startsWithNumber' nt
    where
        startsWithNumber' [] _ = Nothing
        startsWithNumber' ((w, d):ns) s = case w `isPrefixOf` s of
            True -> Just d
            False -> startsWithNumber' ns s

firstDigit :: NumberTable -> String -> Int
firstDigit nt s = case startsWithNumber nt s of
    Just d  -> d
    Nothing -> firstDigit' s 
  where
    firstDigit' [] = error "empty string"
    firstDigit' (c:cs) = if (isDigit c) then (read [c]) else firstDigit nt cs



lastDigit :: NumberTable -> String -> Int
lastDigit nt = (firstDigit nt) . reverse

calibValue :: String -> Int
calibValue s = let
        d1 = firstDigit numbers s
        d2 = lastDigit reversedNumbers s
    in
        d1*10 + d2
