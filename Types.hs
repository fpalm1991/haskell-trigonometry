module Types (Quadrant (..), AngleType (..), (~=), epsilon) where

data Quadrant = Q1 | Q2 | Q3 | Q4 | Origin | OnXAxis | OnYAxis
  deriving (Eq, Show)

data AngleType = Degrees | Radians
  deriving (Eq, Show)

epsilon :: Double
epsilon = 1e-10

(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < epsilon
