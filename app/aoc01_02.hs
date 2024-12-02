module Main where

import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)

locationsFromLine :: String -> Maybe (Integer, Integer)
locationsFromLine line =
  case map read (words line) of
    (x0 : x1 : _) -> Just (x0, x1)
    _ -> Nothing

main :: IO ()
main = do
  contents <- getContents

  let locations = map locationsFromLine $ lines contents
  let (la, lb) :: ([Integer], [Integer]) =
        foldr (\(a, b) (lat, lbt) -> (a : lat, b : lbt)) ([], []) (catMaybes locations)

  let sums :: Map.Map Integer Integer = Map.fromListWith (+) (map (\x -> (x, x)) lb)

  let total :: Integer =
        foldr (+) 0 $
          map
            ( \x -> case Map.lookup x sums of
                Just x0 -> x0
                Nothing -> 0
            )
            la
  print total
