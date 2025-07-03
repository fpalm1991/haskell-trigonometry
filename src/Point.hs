module Point (Point (..), getX, getY) where

type X = Double

type Y = Double

data Point = Point X Y deriving (Show, Eq, Ord)

getX :: Point -> X
getX (Point x _) = x

getY :: Point -> Y
getY (Point _ y) = y
