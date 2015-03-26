
module NumericalFunctions where


-- ===========> Internal Modules <=============
import Types

--  ==============> <===============

-- | Indexes begin at zero
calculateBond :: Int -> Int -> MolCoord -> Double 
calculateBond a1 a2 mol = bond (mol !! i) (mol !! j)
 where [i,j]= map pred [a1,a2]

bond :: Vec3D -> Vec3D -> Double
bond xs ys = sqrt . sum $ zipWith (\x y -> (x-y)^2) xs ys
