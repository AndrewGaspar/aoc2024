module Main where

import Data.List
import Data.Maybe

locationsFromLine :: String -> Maybe (Integer, Integer)
locationsFromLine line =
  case map read (words line) of
    (x0 : x1 : _) -> Just (x0, x1)
    _ -> Nothing

main :: IO ()
main = do
  contents <- getContents

  let locations = map locationsFromLine $ lines contents
  let (la, lb) :: ([Integer], [Integer]) = foldr (\(a, b) (lat, lbt) -> (a : lat, b : lbt)) ([], []) (catMaybes locations)

  let total :: Integer = sum $ zipWith (\a b -> abs (a - b)) (sort la) (sort lb)

  print total
