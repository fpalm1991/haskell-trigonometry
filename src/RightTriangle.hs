module RightTriangle (RightTriangle (..), Side (..), SideName (..), AngleName (..), Angle' (..), fromSideAngle, fromTwoSides, rightTriangle) where

import Angle
import InverseTrigFunctions
import TrigFunctions
import Types

data Side = Side
  { sideLength :: Double,
    sideName :: SideName
  }
  deriving (Show)

data AngleName = NonRightAngle | RightAngle deriving (Show)

data Angle' = Angle'
  { angle :: Angle,
    angleName :: AngleName
  }
  deriving (Show)

data RightTriangle = RightTriangle
  { adjacent :: Side,
    opposite :: Side,
    hypotenuse :: Side,
    angleA :: Angle',
    angleB :: Angle',
    angleC :: Angle'
  }
  deriving (Show)

data RightTriangleInput = FromTwoSides Side Side | FromSideAngle Side Angle' deriving (Show)

fromSideAngle :: Side -> Angle' -> RightTriangleInput
fromSideAngle = FromSideAngle

fromTwoSides :: Side -> Side -> RightTriangleInput
fromTwoSides = FromTwoSides

rightTriangle :: RightTriangleInput -> Maybe RightTriangle
rightTriangle (FromSideAngle s1 a1) = createRightTriangleFrom s1 a1
rightTriangle (FromTwoSides s1 s2) = createRightTriangleFrom' s1 s2

createRightTriangleFrom :: Side -> Angle' -> Maybe RightTriangle
createRightTriangleFrom (Side sideValue Hypotenuse) (Angle' (Angle angleValue Degrees) NonRightAngle) = Just rightTriangle
  where
    adj = Side (cos' (Angle angleValue Degrees) * sideValue) Adjacent
    opp = Side (sin' (Angle angleValue Degrees) * sideValue) Opposite
    hyp = Side sideValue Hypotenuse
    angleA = Angle' (Angle angleValue Degrees) NonRightAngle
    angleB = Angle' (Angle (180 - 90 - angleValue) Degrees) NonRightAngle
    angleC = Angle' (Angle 90 Degrees) RightAngle
    rightTriangle = RightTriangle adj opp hyp angleA angleB angleC
createRightTriangleFrom (Side sideValue Hypotenuse) (Angle' (Angle angleValue Radians) NonRightAngle) = createRightTriangleFrom (Side sideValue Hypotenuse) (Angle' (convertAngle $ Angle angleValue Radians) NonRightAngle)
createRightTriangleFrom (Side sideValue Opposite) (Angle' (Angle angleValue Degrees) NonRightAngle) = Just rightTriangle
  where
    adj = case tan' (Angle angleValue Degrees) of
      Just t -> Side (t * sideValue) Adjacent
      Nothing -> error "Invalid angle: tangent is undefined."
    opp = Side sideValue Opposite
    hyp = Side (sideValue / sin' (Angle angleValue Degrees)) Hypotenuse
    angleA = Angle' (Angle angleValue Degrees) NonRightAngle
    angleB = Angle' (Angle (180 - 90 - angleValue) Degrees) NonRightAngle
    angleC = Angle' (Angle 90 Degrees) RightAngle
    rightTriangle = RightTriangle adj opp hyp angleA angleB angleC
createRightTriangleFrom (Side sideValue Opposite) (Angle' (Angle angleValue Radians) NonRightAngle) = createRightTriangleFrom (Side sideValue Opposite) (Angle' (convertAngle $ Angle angleValue Radians) NonRightAngle)
createRightTriangleFrom (Side sideValue Adjacent) (Angle' (Angle angleValue Degrees) NonRightAngle) = Just rightTriangle
  where
    adj = Side sideValue Adjacent
    opp = case tan' (Angle angleValue Degrees) of
      Just t -> Side (t * sideValue) Opposite
      Nothing -> error "Invalid angle: tangent is undefined."
    hyp = Side (sideValue / cos' (Angle angleValue Degrees)) Hypotenuse
    angleA = Angle' (Angle angleValue Degrees) NonRightAngle
    angleB = Angle' (Angle (180 - 90 - angleValue) Degrees) NonRightAngle
    angleC = Angle' (Angle 90 Degrees) RightAngle
    rightTriangle = RightTriangle adj opp hyp angleA angleB angleC
createRightTriangleFrom (Side sideValue Adjacent) (Angle' (Angle angleValue Radians) NonRightAngle) = createRightTriangleFrom (Side sideValue Adjacent) (Angle' (convertAngle $ Angle angleValue Radians) NonRightAngle)

createRightTriangleFrom' :: Side -> Side -> Maybe RightTriangle
createRightTriangleFrom' adjacent@(Side adj Adjacent) opposite@(Side opp Opposite) = case arctan (opp / adj) of
  Left err -> error err
  Right angle ->
    let hyp = sqrt $ adj ^ 2 + opp ^ 2
        angleA = Angle' (convertAngle angle) NonRightAngle
        angleB = Angle' (Angle (90 - value (convertAngle angle)) Degrees) NonRightAngle
        angleC = Angle' (Angle 90 Degrees) RightAngle
        rightTriangle = RightTriangle adjacent opposite (Side hyp Hypotenuse) angleA angleB angleC
     in Just rightTriangle
createRightTriangleFrom' adjacent@(Side adj Adjacent) hypotenuse@(Side hyp Hypotenuse) = case arccos (adj / hyp) of
  Left err -> error err
  Right θ ->
    let opp = sqrt $ hyp ^ 2 - adj ^ 2
        angleA = Angle' (convertAngle θ) NonRightAngle
        angleB = Angle' (Angle (90 - value (convertAngle θ)) Degrees) NonRightAngle
        angleC = Angle' (Angle 90 Degrees) RightAngle
        rightTriangle = RightTriangle adjacent (Side opp Opposite) hypotenuse angleA angleB angleC
     in Just rightTriangle
createRightTriangleFrom' opposite@(Side opp Opposite) hypotenuse@(Side hyp Hypotenuse) = case arcsin (opp / hyp) of
  Left err -> error err
  Right θ ->
    let adj = sqrt $ hyp ^ 2 - opp ^ 2
        angleA = Angle' (convertAngle θ) NonRightAngle
        angleB = Angle' (Angle (90 - value (convertAngle θ)) Degrees) NonRightAngle
        angleC = Angle' (Angle 90 Degrees) RightAngle
        rightTriangle = RightTriangle (Side adj Adjacent) opposite hypotenuse angleA angleB angleC
     in Just rightTriangle
