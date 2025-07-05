module Main where

import Angle
import RightTriangle
import Types

main :: IO ()
main = do
  -- Right Triangle
  print (rightTriangle (fromSideAngle (Side 21 Hypotenuse) (Angle' (Angle 25 Degrees) NonRightAngle)))
  print (rightTriangle (fromTwoSides (Side 4 Opposite) (Side 12 Hypotenuse)))
