module RightTriangle (RightTriangle (..), Side (..), SideName (..), AngleName (..), Angle' (..), createFrom) where

import Angle
import TrigFunctions
import Types

data SideName = Adjacent | Opposite | Hypotenuse deriving (Show)

data Side = Side
  { value :: Double,
    unit :: SideName
  }
  deriving (Show)

data AngleName = AngleA | AngleB | AngleC deriving (Show)

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

createFrom :: Side -> Angle' -> RightTriangle
createFrom (Side sideValue Hypotenuse) (Angle' (Angle angleValue Degrees) AngleA) = RightTriangle adj opp hyp angleA angleB angleC
  where
    adj = Side (cos' (Angle angleValue Degrees) * sideValue) Adjacent
    opp = Side (sin' (Angle angleValue Degrees) * sideValue) Opposite
    hyp = Side sideValue Hypotenuse
    angleA = Angle' (Angle angleValue Degrees) AngleA
    angleB = Angle' (Angle (180 - 90 - angleValue) Degrees) AngleB
    angleC = Angle' (Angle 90 Degrees) AngleC
createFrom (Side sideValue Hypotenuse) (Angle' (Angle angleValue Radians) AngleA) = createFrom (Side sideValue Hypotenuse) (Angle' (convertAngle $ Angle angleValue Radians) AngleA)

-- createFromOppositeAndAngle
-- createFromAdjacentAndAngle
-- createFromTwoSides
