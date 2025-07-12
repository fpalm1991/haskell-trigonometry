module Angle.Angle (Unit (..), Angle (..), convert, normalize) where

import Data.Fixed as DF
import Util.Functions

data Unit = Radians | Degrees deriving (Show)

data Angle = Angle
  { value :: Double,
    unit :: Unit
  }

instance Show Angle where
  show (Angle v Radians) = show v ++ " " ++ "rad"
  show (Angle v Degrees) = show v ++ " " ++ "°"

convert :: Angle -> Angle
convert (Angle v Radians) = Angle (v * 180 / pi) Degrees
convert (Angle v Degrees) = Angle (v * pi / 180) Radians

normalize :: Angle -> Angle
normalize angle@(Angle _ Radians) = normalizeRadians angle
normalize angle@(Angle _ Degrees) = normalizeDegrees angle

normalizeRadians :: Angle -> Angle
normalizeRadians (Angle v Radians)
  | v ≈ 0 = Angle 0 Radians
  | v `DF.mod'` (2 * pi) ≈ 0 = Angle (2 * pi) Radians
  | otherwise = Angle (v `DF.mod'` (2 * pi)) Radians
normalizeRadians angle@(Angle _ Degrees) = normalizeDegrees . convert $ angle

normalizeDegrees :: Angle -> Angle
normalizeDegrees (Angle v Degrees)
  | v ≈ 0 = Angle 0 Degrees
  | v `DF.mod'` 360 ≈ 0 = Angle 360 Degrees
  | otherwise = Angle (v `DF.mod'` 360) Degrees
normalizeDegrees angle@(Angle _ Radians) = normalizeDegrees . convert $ angle
