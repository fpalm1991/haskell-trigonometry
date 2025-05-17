module Types (Quadrant (..), AngleType (..)) where

data Quadrant = Q1 | Q2 | Q3 | Q4 | Origin | OnXAxis | OnYAxis
  deriving (Show)

data AngleType = Degrees | Radians
  deriving (Eq, Show)
