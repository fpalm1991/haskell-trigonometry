module AngleFunctions (
    toRadians,
    toDegrees,
    normalizeAngle,
    getAllCoterminalAngles,
    determineQuadrantPoint,
    determineQuadrantAngle,
    findReferenceAngle) where

    import Point
    import Types
    import Data.Fixed (mod')

    toRadians :: Degrees -> Radians
    toRadians degree = degree * (pi / 180)

    toDegrees :: Radians -> Degrees
    toDegrees radians = radians * (180 / pi)

    -- Normalizes an angle to the range [0, 360) for degrees
    -- or [0, 2π) for radians, effectively removing full rotations.
    normalizeAngle :: Double -> AngleType -> Double
    normalizeAngle val angleType
        | angleType == InDegrees = val `mod'` 360
        | angleType == InRadians = val `mod'` (2 * pi)

    -- Given an angle in degrees or radians, this function returns all
    -- positive coterminal angles less than the absolute value of the input.
    -- The input angle itself is excluded from the result.
    -- Negative angles are treated as their absolute value.
    getAllCoterminalAngles :: Double -> AngleType -> [Double]
    getAllCoterminalAngles angle angleType            
        | angleType == InDegrees = reverse $ filter (\a -> a > 0 && a /= angleAbs) [angleAbs, angleAbs - 360 .. 0]
        | angleType == InRadians = reverse $ filter (\a -> a > 0 && a /= angleAbs) [angleAbs, angleAbs - 2 * pi .. 0]
            where
                angleAbs = abs angle

    determineQuadrantPoint :: Point -> Quadrant
    determineQuadrantPoint (Point 0 0) = Origin
    determineQuadrantPoint (Point 0 _) = OnYAxis
    determineQuadrantPoint (Point _ 0) = OnXAxis
    determineQuadrantPoint point
        | x > 0 && y > 0 = Q1
        | x < 0 && y > 0 = Q2
        | x < 0 && y < 0 = Q3
        | x > 0 && y < 0 = Q4
            where
                x = getX point
                y = getY point

    determineQuadrantAngleRadians :: Radians -> Quadrant
    determineQuadrantAngleRadians radians
        | radiansNormalized < pi / 2 = Q1
        | radiansNormalized == pi / 2 = OnYAxis
        | radiansNormalized < pi = Q2
        | radiansNormalized == pi = OnXAxis
        | radiansNormalized < 3 / 2 * pi = Q3
        | radiansNormalized == 3 / 2 * pi = OnYAxis
        | radiansNormalized < 2 * pi = Q4
        | radiansNormalized == 2 * pi = OnXAxis
            where
                radiansNormalized = normalizeAngle radians InRadians

    determineQuadrantAngleDegrees :: Degrees -> Quadrant
    determineQuadrantAngleDegrees degrees
        | degreesNormalized < 90 = Q1
        | degreesNormalized == 90 = OnYAxis
        | degreesNormalized < 180 = Q2
        | degreesNormalized == 180 = OnXAxis
        | degreesNormalized < 270 = Q3
        | degreesNormalized == 270 = OnYAxis
        | degreesNormalized < 360 = Q4
        | degreesNormalized == 360 = OnXAxis
            where
                degreesNormalized = normalizeAngle degrees InDegrees

    determineQuadrantAngle :: Double -> AngleType -> Quadrant
    determineQuadrantAngle angle angleType
        | angleType == InDegrees = determineQuadrantAngleDegrees angle
        | angleType == InRadians = determineQuadrantAngleRadians angle

    findReferenceAngle :: Double -> AngleType -> Double
    findReferenceAngle angle angleType
        | angleType == InDegrees
            = if a <= 90 then a
            else if a <= 180 then 180 - a
            else if a <= 270 then a - 180
            else 360 - a
        | angleType == InRadians
            = if a <= pi / 2 then a
            else if a <= pi then pi - a
            else if a <= 3 * pi / 2 then a - pi
            else 2 * pi - a
                where
                    a = normalizeAngle angle angleType
