{-
This file is part of my solutions for Advent of Code 2023.
Copyright (C) 2024  Luka Berkers

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

module Day2Spec (
    day2Spec,
) where

import Day2 (CubeSet (..), cubeThresholds)
import Test.Hspec (SpecWith, describe, it, shouldBe)

day2Spec :: SpecWith ()
day2Spec = describe "Day2" $ do
    cubeThresholdsSpec

cubeThresholdsSpec :: SpecWith ()
cubeThresholdsSpec = describe "cubeThresholds" $ do
    it "should give exactly the expected thresholds" $ do
        let actual = cubeThresholds
        csRedCount actual `shouldBe` 12
        csGreenCount actual `shouldBe` 13
        csBlueCount actual `shouldBe` 14
