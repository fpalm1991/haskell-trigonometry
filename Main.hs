module Main where

import Angle
import AngleFunctions
import Point
import Types

main :: IO ()
main = do
  -- Basic Angle Functions
  print (toRadians 180) -- should print ~3.1415 (pi)
  print (normalizeAngle (2 * pi + pi / 2) InRadians) -- should print ~1.57
  print (getAllCoterminalAngles (4 * pi) InRadians) -- shoud print 2 * pi
  print (determineQuadrantPoint (Point (-4) 4)) -- should print Q2 (Quadrant II)
  print (determineQuadrantAngle (pi / 2) InRadians) -- should print OnYAxis
  print (findReferenceAngle (-(pi / 4)) InRadians) -- should print pi / 4
  print (findReferenceAngle 170 InDegrees) -- should print 10

  -- Angle data type leveraging Semigroup and Monoid for composition
  print (Angle (-90) InDegrees <> Angle 0 InRadians) -- should print ~4.71
  print (mconcat [Angle 90 InDegrees, Angle pi InRadians, Angle 270 InDegrees]) -- should print ~3.14 (pi)
