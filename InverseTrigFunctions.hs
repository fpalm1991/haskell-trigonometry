module InverseTrigFunctions (arcsin, arccos, arctan) where

import Angle
import Types

arcsin :: Double -> Maybe Angle
arcsin value
  | -1 <= value && value <= 1 = Just $ Angle (asin value) Radians
  | otherwise = Nothing

arccos :: Double -> Maybe Angle
arccos value
  | -1 <= value && value <= 1 = Just $ Angle (acos value) Radians
  | otherwise = Nothing

arctan :: Double -> Maybe Angle
arctan value = Just $ Angle (atan value) Radians
