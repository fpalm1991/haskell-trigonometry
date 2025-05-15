module UnitCircle (getCoordinatesOnUnitCircle) where

import Angle
import AngleFunctions
import Types

getCoordinatesOnUnitCircle :: Angle -> (Double, Double)
getCoordinatesOnUnitCircle (Angle value InRadians) = (cos value, sin value)
getCoordinatesOnUnitCircle (Angle value InDegrees) = (cos $ toRadians value, sin $ toRadians value)
