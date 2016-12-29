module Util where

import Prelude
import Data.Array (range)
import Data.Ratio (Ratio(..), gcd)


lcm :: Int -> Int -> Int
lcm a b = (a * b) / gcd (Ratio a b)

generatePolyrhythm :: Int -> Int -> Array (Array Boolean)
generatePolyrhythm a b =
  [ map (\i -> i `mod` (lcm a b / a) == 0) (range 0 (lcm a b - 1))
  , map (\i -> i `mod` (lcm a b / b) == 0) (range 0 (lcm a b - 1))
  ]
