module Main (main) where

import Angle
import InverseTrigFunctions
import Point
import Test.Hspec
import TrigFunctions
import Types

main :: IO ()
main = hspec $ do
  describe "Angle" $ do
    it "converts an angle from radians to degrees" $ do
      convertAngle (Angle 90 Degrees) `shouldBe` Angle (pi / 2) Radians

    it "converts an angle from radians to degrees" $ do
      convertAngle (Angle pi Radians) `shouldBe` Angle 180 Degrees

    it "normalizes an angle in radians to the range [0, 2π)" $ do
      normalizeAngle (Angle (2 * pi + pi / 2) Radians) `shouldBe` Angle (pi / 2) Radians

    it "finds all coterminal angles" $ do
      getAllCoterminalAngles (8 * pi) Radians `shouldBe` [2 * pi, 4 * pi, 6 * pi]

    it "determines the quadrant given a point" $ do
      determineQuadrantPoint (Point (-4) 4) `shouldBe` Q2

    it "determines the quadrant given an angle" $ do
      determineQuadrantAngle (Angle 5 Radians) `shouldBe` Q4

    it "finds the reference angle" $ do
      findReferenceAngle (-(pi / 4)) Radians `shouldBe` Angle (pi / 4) Radians

    it "adds to angles using the Semigroup instance" $ do
      (Angle (-90) Degrees <> Angle (0.5 * pi) Radians) `shouldBe` Angle 0 Radians

    it "sums a list of angles using mconcat" $ do
      mconcat [Angle 90 Degrees, Angle pi Radians, Angle 270 Degrees] `shouldBe` Angle pi Radians

  describe "TrigFunctions" $ do
    it "calculates sine" $ do
      sin' (Angle (pi / 2) Radians) `shouldBe` 1

    it "calculates cosine" $ do
      cos' (Angle 270 Degrees) `shouldSatisfy` (<= epsilon)

    it "calculates tangent" $ do
      tan' (Angle (pi / 4) Radians) `shouldSatisfy` maybe False (\x -> abs (x - 1) <= epsilon)

  describe "InverseTrigFunctions" $ do
    it "calculates arcsin" $ do
      arcsin 0 `shouldBe` Right (Angle 0 Radians)

    it "calculates arccos" $ do
      arccos (-1) `shouldBe` Right (Angle pi Radians)

    it "calculates arctan" $ do
      arctan 1 `shouldBe` Right (Angle (pi / 4) Radians)
