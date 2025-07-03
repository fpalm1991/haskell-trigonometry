module InverseTrigFunctions (arcsin, arccos, arctan) where

import Angle
import Types

arcsin :: Double -> Either String Angle
arcsin value
  | -1 <= value && value <= 1 = Right $ Angle (asin value) Radians
  | otherwise = Left "Input outside domain [-1, 1]"

arccos :: Double -> Either String Angle
arccos value
  | -1 <= value && value <= 1 = Right $ Angle (acos value) Radians
  | otherwise = Left "Input outside domain [-1, 1]"

arctan :: Double -> Either String Angle
arctan value = Right $ Angle (atan value) Radians
