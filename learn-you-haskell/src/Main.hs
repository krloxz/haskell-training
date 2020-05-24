module Main where

import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid

main :: IO ()
main = do
  putStrLn $ "Sphere Area = " ++ show (Sphere.area 1)
  putStrLn $ "Cube Area = " ++ show (Cube.area 1)
  putStrLn $ "Cuboid Area = " ++ show (Cuboid.area 1 1 1)

-- >>> main
-- Sphere Area = 12.566371
-- Cube Area = 6.0
-- Cuboid Area = 6.0
--
