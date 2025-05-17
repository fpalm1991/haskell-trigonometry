module Angle
  ( Angle (..),
    toRadians,
    toDegrees,
    convertAngle,
    normalizeAngleFromValue,
    normalizeAngle,
    determineQuadrantAngle,
    findReferenceAngle,
    getAllCoterminalAngles,
    determineQuadrantPoint,
  )
where

import Data.Fixed (mod')
import Point
import Types

{- Angle Data Type -}

data Angle = Angle
  { value :: Double,
    unit :: AngleType
  }
  deriving (Eq, Show)

-- Angles are always returned in radians
instance Semigroup Angle where
  (<>) (Angle a1 u1) (Angle a2 u2)
    | u1 == Radians && u2 == Radians = Angle (normalizeAngleFromValue (a1 + a2) Radians) Radians
    | u1 == Degrees && u2 == Radians = Angle (normalizeAngleFromValue (toRadians a1 + a2) Radians) Radians
    | u1 == Radians && u2 == Degrees = Angle (normalizeAngleFromValue (a1 + toRadians a2) Radians) Radians
    | u1 == Degrees && u2 == Degrees = Angle (normalizeAngleFromValue (toRadians a1 + toRadians a2) Radians) Radians
    | otherwise = error "Unsupported combination of angle units"

instance Monoid Angle where
  mempty = Angle 0 Radians

{- Angle Functions -}

toRadians :: Double -> Double
toRadians degree = degree * (pi / 180)

toDegrees :: Double -> Double
toDegrees radians = radians * (180 / pi)

convertAngle :: Angle -> Angle
convertAngle (Angle value Radians) = Angle (toDegrees value) Degrees
convertAngle (Angle value Degrees) = Angle (toRadians value) Radians

-- Normalizes an angle to the range [0, 360) for degrees
-- or [0, 2π) for radians, effectively removing full rotations.
normalizeAngleFromValue :: Double -> AngleType -> Double
normalizeAngleFromValue val angleType
  | angleType == Degrees = val `mod'` 360
  | angleType == Radians = val `mod'` (2 * pi)

normalizeAngle :: Angle -> Angle
normalizeAngle (Angle value unit) = Angle (normalizeAngleFromValue value unit) unit

-- Given an angle in degrees or radians, this function returns all
-- positive coterminal angles less than the absolute value of the input.
-- The input angle itself is excluded from the result.
-- Negative angles are treated as their absolute value.
getAllCoterminalAngles :: Double -> AngleType -> [Double]
getAllCoterminalAngles angle angleType
  | angleType == Degrees = reverse $ filter (\a -> a > 0 && a /= angleAbs) [angleAbs, angleAbs - 360 .. 0]
  | angleType == Radians = reverse $ filter (\a -> a > 0 && a /= angleAbs) [angleAbs, angleAbs - 2 * pi .. 0]
  where
    angleAbs = abs angle

determineQuadrantPoint :: Point -> Quadrant
determineQuadrantPoint (Point 0 0) = Origin
determineQuadrantPoint (Point 0 _) = OnYAxis
determineQuadrantPoint (Point _ 0) = OnXAxis
determineQuadrantPoint point
  | x > 0 && y > 0 = Q1
  | x < 0 && y > 0 = Q2
  | x < 0 && y < 0 = Q3
  | x > 0 && y < 0 = Q4
  where
    x = getX point
    y = getY point

determineQuadrantAngleRadians :: Double -> Quadrant
determineQuadrantAngleRadians radians
  | radiansNormalized < pi / 2 = Q1
  | radiansNormalized == pi / 2 = OnYAxis
  | radiansNormalized < pi = Q2
  | radiansNormalized == pi = OnXAxis
  | radiansNormalized < 3 / 2 * pi = Q3
  | radiansNormalized == 3 / 2 * pi = OnYAxis
  | radiansNormalized < 2 * pi = Q4
  | radiansNormalized == 2 * pi = OnXAxis
  where
    radiansNormalized = normalizeAngleFromValue radians Radians

determineQuadrantAngleDegrees :: Double -> Quadrant
determineQuadrantAngleDegrees degrees
  | degreesNormalized < 90 = Q1
  | degreesNormalized == 90 = OnYAxis
  | degreesNormalized < 180 = Q2
  | degreesNormalized == 180 = OnXAxis
  | degreesNormalized < 270 = Q3
  | degreesNormalized == 270 = OnYAxis
  | degreesNormalized < 360 = Q4
  | degreesNormalized == 360 = OnXAxis
  where
    degreesNormalized = normalizeAngleFromValue degrees Degrees

determineQuadrantAngle :: Double -> AngleType -> Quadrant
determineQuadrantAngle angle angleType
  | angleType == Degrees = determineQuadrantAngleDegrees angle
  | angleType == Radians = determineQuadrantAngleRadians angle

findReferenceAngle :: Double -> AngleType -> Double
findReferenceAngle angle angleType
  | angleType == Degrees = case determineQuadrantAngle angle angleType of
      Q1 -> normalized
      Q2 -> 180 - normalized
      Q3 -> normalized - 180
      Q4 -> 360 - normalized
      _ -> 0
  | angleType == Radians = case determineQuadrantAngle angle angleType of
      Q1 -> normalized
      Q2 -> pi - normalized
      Q3 -> normalized - pi
      Q4 -> 2 * pi - normalized
      _ -> 0
  where
    normalized = normalizeAngleFromValue angle angleType
