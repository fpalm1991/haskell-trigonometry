module Angle (Angle (..)) where

import AngleFunctions
import Types

data Angle = Angle
  { value :: Double,
    unit :: AngleType
  }
  deriving (Eq, Show)

-- Angle are always returned in radians
instance Semigroup Angle where
  (<>) (Angle a1 u1) (Angle a2 u2)
    | u1 == InRadians && u2 == InRadians = Angle (normalizeAngle (a1 + a2) InRadians) InRadians
    | u1 == InDegrees && u2 == InRadians = Angle (normalizeAngle (toRadians a1 + a2) InRadians) InRadians
    | u1 == InRadians && u2 == InDegrees = Angle (normalizeAngle (a1 + toRadians a2) InRadians) InRadians
    | u1 == InDegrees && u2 == InDegrees = Angle (normalizeAngle (toRadians a1 + toRadians a2) InRadians) InRadians
    | otherwise = error "Unsupported combination of angle units"

instance Monoid Angle where
  mempty = Angle 0 InRadians
