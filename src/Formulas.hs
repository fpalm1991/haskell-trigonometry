module Formulas (pythagoras) where

import RightTriangle

{- Pythagorean theorem -}

pythagoras :: (SideName, Double) -> (SideName, Double) -> Maybe (SideName, Double)
pythagoras (Opposite, o) (Adjacent, a) = Just (Hypotenuse, sqrt $ o ^ (2 :: Integer) + a ^ (2 :: Integer))
pythagoras (Adjacent, a) (Opposite, o) = pythagoras (Opposite, o) (Adjacent, a)
pythagoras (Opposite, o) (Hypotenuse, h) = Just (Adjacent, sqrt $ h ^ (2 :: Integer) - o ^ (2 :: Integer))
pythagoras (Hypotenuse, h) (Opposite, o) = pythagoras (Opposite, o) (Hypotenuse, h)
pythagoras (Adjacent, a) (Hypotenuse, h) = Just (Opposite, sqrt $ h ^ (2 :: Integer) - a ^ (2 :: Integer))
pythagoras (Hypotenuse, h) (Adjacent, a) = pythagoras (Adjacent, a) (Hypotenuse, h)
pythagoras _ _ = Nothing
