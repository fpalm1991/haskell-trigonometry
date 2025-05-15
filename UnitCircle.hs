module UnitCircle (getCoordinatesOnUnitCircle) where

import Angle
import AngleFunctions
import Point
import Types

getCoordinatesOnUnitCircle :: Angle -> Point
getCoordinatesOnUnitCircle (Angle value InRadians) = Point (cos value) (sin value)
getCoordinatesOnUnitCircle (Angle value InDegrees) = Point (cos $ toRadians value) (sin $ toRadians value)
