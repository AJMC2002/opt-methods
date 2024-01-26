module Main where

import Data.Massiv.Array as A
import Formst
import MatrixGame (highStrategy, lowStrategy)
import Optimization.DirectTask (solveDirect)
import Optimization.DualTask (solveDual)
import Prelude as P
import System.Random qualified as R

main :: IO ()
main = do
    let 
        -- Initial values
        salt = 19092002
        gen = R.mkStdGen salt
        rng = (-50 :: Double, 50)
        comp = ParN 0
        a = computeP $ uniformRangeArray gen rng comp (Sz2 6 8) :: Matrix P Double
        b = A.replicate (ParN 0) 6 1
        c = A.replicate (ParN 0) 8 1
        beta = minimum' a
        a' = a .+ abs beta

    putStrLn $ showMat a 2
    putStrLn $ "underline(A) = " ++ show (lowStrategy a)
    putStrLn $ "overline(A) = " ++ show (highStrategy a)
    putStrLn $ "beta = " ++ show beta
    putStrLn $ showMat a' 2

    (y, alfa1) <- solveDirect a' b c
    putStrLn $ "y = " ++ showVec y 2
    putStrLn $ "alfa = " ++ show alfa1
    putStrLn $ "q^* = y/||y|| = " ++ showVec (y ./ normL2 y) 2

    (x, alfa2) <- solveDual a' b c
    putStrLn $ "x = " ++ showVec x 2
    putStrLn $ "alfa = " ++ show alfa2
    putStrLn $ "p^* = x/||x|| = " ++ showVec (x ./ normL2 x) 2

    putStrLn $ "underline(A) = " ++ show (lowStrategy a)
    putStrLn $ "overline(A) = " ++ show (highStrategy a)
    putStrLn $ "phi = " ++ show (1 / alfa1 - abs beta)
