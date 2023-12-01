module Main where

import System.Environment
import qualified MyLib (calibValue)

main :: IO ()
main = getArgs >>= parse 

parse :: [String] -> IO ()

parse ["--part1", path] = do
  --putStrLn $ show $ MyLib.startsWithNumber "eightwothree"
  input <- readFile path
  let vals = map MyLib.calibValue $ lines input
  putStrLn $ show $ sum vals

parse _ = do
  putStrLn "you are wrong, sorry!"

