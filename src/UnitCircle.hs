module UnitCircle (getCoordinatesOnUnitCircle) where

import Angle
import Point
import Types

getCoordinatesOnUnitCircle :: Angle -> Point
getCoordinatesOnUnitCircle (Angle value Radians) = Point (cos value) (sin value)
getCoordinatesOnUnitCircle (Angle value Degrees) = Point (cos $ toRadians value) (sin $ toRadians value)
