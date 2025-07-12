module Main where

import Angle.Angle

main :: IO ()
main = do
  print $ normalize (Angle 380 Degrees)
