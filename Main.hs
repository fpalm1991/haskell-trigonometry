module Main where

import Angle
import Point
import RightTriangle
import TrigFunctions
import Types
import UnitCircle

main :: IO ()
main = do
  -- Basic Angle Functions
  print (toRadians 180) -- should print ~3.1415 (pi)
  print (convertAngle (Angle 90 Degrees)) -- should print ~1.57 (pi/2)
  print (convertAngle (Angle pi Radians)) -- should print 180
  print (normalizeAngleFromValue (2 * pi + pi / 2) Radians) -- should print ~1.57 (pi/2)
  print (normalizeAngle (Angle (2 * pi + pi / 2) Radians)) -- should print ~1.57 (pi/2)
  print (getAllCoterminalAngles (4 * pi) Radians) -- shoud print 2 * pi
  print (determineQuadrantPoint (Point (-4) 4)) -- should print Q2 (Quadrant II)
  print (determineQuadrantAngleFromValue (pi / 2) Radians) -- should print OnYAxis
  print (findReferenceAngle (-(pi / 4)) Radians) -- should print pi / 4
  print (findReferenceAngle 170 Degrees) -- should print 10

  -- Angle data type leveraging Semigroup and Monoid for composition
  print (Angle (-90) Degrees <> Angle 0 Radians) -- should print ~4.71
  print (mconcat [Angle 90 Degrees, Angle pi Radians, Angle 270 Degrees]) -- should print ~3.14 (pi)

  -- Unit Circle
  print (getCoordinatesOnUnitCircle (Angle 90 Degrees))
  print (getCoordinatesOnUnitCircle (Angle (3 * pi / 2) Radians))

  -- Trig Functions
  print (tan' (Angle (pi / 4) Radians)) -- should print 1
  print (csc' (Angle 360 Degrees)) -- should print "Nothing"

  -- Right Triangle
  print (createRightTriangleFrom (Side 21 Hypotenuse) (Angle' (Angle 25 Radians) NonRightAngle))
  print (createRightTriangleFrom (Side 10 Opposite) (Angle' (Angle 30 Degrees) NonRightAngle))
  print (createRightTriangleFrom (Side 8 Adjacent) (Angle' (Angle 45 Degrees) NonRightAngle))
