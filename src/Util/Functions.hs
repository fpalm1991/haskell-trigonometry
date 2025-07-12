module Util.Functions ((≈)) where

(≈) :: (Fractional a, Ord a) => a -> a -> Bool
(≈) x y = abs (x - y) <= 1e-10
