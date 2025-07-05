module Angle
  ( Angle (..),
    toRadians,
    toDegrees,
    convertAngle,
    normalizeAngleFromValue,
    normalizeAngle,
    determineQuadrantAngleFromValue,
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
convertAngle (Angle v Radians) = Angle (toDegrees v) Degrees
convertAngle (Angle v Degrees) = Angle (toRadians v) Radians

-- Normalizes an angle to the range [0, 360) for degrees
-- or [0, 2π) for radians, effectively removing full rotations.
normalizeAngleFromValue :: Double -> AngleType -> Double
normalizeAngleFromValue val angleType
  | angleType == Degrees = val `mod'` 360
  | angleType == Radians = val `mod'` (2 * pi)
  | otherwise = error "No valid angle type."

normalizeAngle :: Angle -> Angle
normalizeAngle (Angle v u) = Angle (normalizeAngleFromValue v u) u

-- Given an angle in degrees or radians, this function returns all
-- positive coterminal angles less than the absolute value of the input.
-- The input angle itself is excluded from the result.
-- Negative angles are treated as their absolute value.
getAllCoterminalAngles :: Double -> AngleType -> [Double]
getAllCoterminalAngles angle angleType
  | angleType == Degrees = reverse $ filter (\a -> a > 0 && a /= angleAbs) [angleAbs, angleAbs - 360 .. 0]
  | angleType == Radians = reverse $ filter (\a -> a > 0 && a /= angleAbs) [angleAbs, angleAbs - 2 * pi .. 0]
  | otherwise = error "No valid angle type."
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
  | otherwise = error "No valid point."
  where
    x = getX point
    y = getY point

determineQuadrantAngleRadians :: Double -> Quadrant
determineQuadrantAngleRadians radians
  | radiansNormalized ~= 0 || radiansNormalized ~= pi || radiansNormalized ~= (2 * pi) = OnXAxis
  | radiansNormalized ~= (pi / 2) || radiansNormalized ~= (3 / 2 * pi) = OnYAxis
  | radiansNormalized < pi / 2 = Q1
  | radiansNormalized < pi = Q2
  | radiansNormalized < (3 / 2 * pi) = Q3
  | radiansNormalized < 2 * pi = Q4
  | otherwise = error "No valid degree value."
  where
    radiansNormalized = normalizeAngleFromValue radians Radians

determineQuadrantAngleDegrees :: Double -> Quadrant
determineQuadrantAngleDegrees degrees
  | degreesNormalized ~= 0 || degreesNormalized ~= 180 || degreesNormalized ~= 360 = OnXAxis
  | degreesNormalized ~= 90 || degreesNormalized ~= 270 = OnYAxis
  | degreesNormalized < 90 = Q1
  | degreesNormalized < 180 = Q2
  | degreesNormalized < 270 = Q3
  | degreesNormalized < 360 = Q4
  | otherwise = error "No valid degree value."
  where
    degreesNormalized = normalizeAngleFromValue degrees Degrees

determineQuadrantAngleFromValue :: Double -> AngleType -> Quadrant
determineQuadrantAngleFromValue angle angleType
  | angleType == Degrees = determineQuadrantAngleDegrees angle
  | angleType == Radians = determineQuadrantAngleRadians angle
  | otherwise = error "No valid angle type."

determineQuadrantAngle :: Angle -> Quadrant
determineQuadrantAngle (Angle value unit) = determineQuadrantAngleFromValue value unit

findReferenceAngle :: Double -> AngleType -> Angle
findReferenceAngle angle angleType
  | angleType == Degrees = case determineQuadrantAngleFromValue angle angleType of
      Q1 -> Angle normalized angleType
      Q2 -> Angle (180 - normalized) angleType
      Q3 -> Angle (normalized - 180) angleType
      Q4 -> Angle (360 - normalized) angleType
      _ -> error "No valid quadrant."
  | angleType == Radians = case determineQuadrantAngleFromValue angle angleType of
      Q1 -> Angle normalized angleType
      Q2 -> Angle (pi - normalized) angleType
      Q3 -> Angle (normalized - pi) angleType
      Q4 -> Angle (2 * pi - normalized) angleType
      _ -> error "No valid quadrant."
  | otherwise = error "No valid angle type."
  where
    normalized = normalizeAngleFromValue angle angleType
