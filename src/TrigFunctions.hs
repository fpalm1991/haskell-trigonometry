module TrigFunctions (sin', cos', tan', sec', csc', cot') where

import Angle
import Types

sin' :: Angle -> Double
sin' (Angle value Radians) = sin value
sin' (Angle value Degrees) = sin $ toRadians value

cos' :: Angle -> Double
cos' (Angle value Radians) = cos value
cos' (Angle value Degrees) = cos $ toRadians value

tan' :: Angle -> Maybe Double
tan' (Angle value unit)
  | quadrant == OnYAxis = Nothing
  | unit == Radians = Just (tan value)
  | unit == Degrees = Just (tan $ toRadians value)
  | otherwise = error "No valid angle."
  where
    quadrant = determineQuadrantAngle (Angle value unit)

sec' :: Angle -> Maybe Double
sec' (Angle value unit)
  | quadrant == OnYAxis = Nothing
  | unit == Radians = Just (1 / cos value)
  | unit == Degrees = Just (1 / cos (toRadians value))
  | otherwise = error "No valid angle."
  where
    quadrant = determineQuadrantAngle (Angle value unit)

csc' :: Angle -> Maybe Double
csc' (Angle value unit)
  | quadrant == OnXAxis = Nothing
  | unit == Radians = Just (1 / sin value)
  | unit == Degrees = Just (1 / sin (toRadians value))
  | otherwise = error "No valid angle."
  where
    quadrant = determineQuadrantAngle (Angle value unit)

cot' :: Angle -> Maybe Double
cot' (Angle value unit)
  | abs tanVal < epsilon = Nothing -- 1 / tan θ
  | abs sinVal < epsilon = Nothing -- cos θ / sin θ
  | otherwise = Just (cosVal / sinVal)
  where
    valueInRadians = if unit == Degrees then toRadians value else value
    tanVal = tan valueInRadians
    sinVal = sin valueInRadians
    cosVal = cos valueInRadians
