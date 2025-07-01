module Formulas (pythagoras) where

import Types

{- Pythagorean theorem -}

pythagoras :: (SideName, Double) -> (SideName, Double) -> Maybe Double
pythagoras (Opposite, o) (Adjacent, a) = Just $ sqrt $ o ^ 2 + a ^ 2
pythagoras (Adjacent, a) (Opposite, o) = pythagoras (Opposite, o) (Adjacent, a)
pythagoras (Opposite, o) (Hypotenuse, h) = Just $ sqrt $ h ^ 2 - o ^ 2
pythagoras (Hypotenuse, h) (Opposite, o) = pythagoras (Opposite, o) (Hypotenuse, h)
pythagoras (Adjacent, a) (Hypotenuse, h) = Just $ sqrt $ h ^ 2 - a ^ 2
pythagoras (Hypotenuse, h) (Adjacent, a) = pythagoras (Adjacent, a) (Hypotenuse, h)
pythagoras _ _ = Nothing
