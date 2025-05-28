module RightTriangle (RightTriangle (..), Side (..), SideName (..), AngleName (..), Angle' (..), createRightTriangleFrom) where

import Angle
import TrigFunctions
import Types

data SideName = Adjacent | Opposite | Hypotenuse deriving (Show)

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
