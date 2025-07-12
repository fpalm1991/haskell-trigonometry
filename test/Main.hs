module Main (main) where

import Angle.Angle
import Test.Hspec

approx :: Angle -> Angle -> Bool
approx (Angle x Radians) (Angle y Radians) = abs (x - y) <= 1e-10
approx (Angle x Degrees) (Angle y Degrees) = abs (x - y) <= 1e-10
approx (Angle x Radians) angle@(Angle _ Degrees) = abs (x - value (convert angle)) <= 1e-10
approx angle@(Angle _ Degrees) (Angle y Radians) = abs (value (convert angle) - y) <= 1e-10

main :: IO ()
main = hspec $ do
  describe "Angle.Angle" $ do
    it "converts angles" $ do
      convert (Angle 0 Degrees) `shouldBe` Angle 0 Radians
      convert (Angle pi Radians) `shouldSatisfy` approx (Angle 180 Degrees)
      convert (Angle 90 Degrees) `shouldSatisfy` approx (Angle (pi / 2) Radians)
      convert (Angle (5 * pi) Radians) `shouldSatisfy` approx (Angle 900 Degrees)
      convert (Angle (-360) Degrees) `shouldSatisfy` approx (Angle ((-2) * pi) Radians)
