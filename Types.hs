module Types (Quadrant (..), AngleType (..), Radians, Degrees) where

type Radians = Double

type Degrees = Double

data Quadrant = Q1 | Q2 | Q3 | Q4 | Origin | OnXAxis | OnYAxis
  deriving (Show)

data AngleType = InDegrees | InRadians
  deriving (Eq, Show)
