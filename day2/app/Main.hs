module Main where

import System.Environment
import MyLib (parseGames, isGamePossible, power)

main :: IO ()
main = getArgs >>= parse 


parse :: [String] -> IO ()

parse ["--part1", path] = do
  input <- readFile path
  case parseGames input of
    Left err -> putStrLn $ show err
    Right gs -> putStrLn $ show $ sum $ map (isGamePossible 12 13 14) gs

parse ["--part2", path] = do
  input <- readFile path
  case parseGames input of
    Left err    -> putStrLn $ show err
    Right games -> putStrLn $ show $ sum $ map power games

parse _ = do
  putStrLn "you are wrong, sorry!"

