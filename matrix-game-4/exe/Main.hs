module Main where

import Data.Massiv.Array as A
import Formst
import MatrixGame (highStrategy, lowStrategy)
import Optimization.DirectTask (solveDirect)
import Optimization.DualTask (solveDual)
import Prelude as P

main :: IO ()
main = do
    let a = resize' (Sz2 6 8) $ fromList (ParN 0) [11, 13, 10, 14, 9, 19, 3, 4, 4, 5, 17, 16, 11, 12, 9, 15, 5, 10, 11, 5, 19, 4, 11, 16, 1, 4, 7, 2, 5, 0, 16, 5, 19, 12, 13, 17, 3, 4, 18, 13, 3, 7, 11, 5, 8, 4, 2, 10]
        b = A.replicate (ParN 0) 6 1
        c = A.replicate (ParN 0) 8 1
        beta = minimum' a
        a' = a .+ beta

    putStrLn $ showMat a 2
    putStrLn $ showVec b 2
    putStrLn $ showVec c 2

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
    putStrLn $ "beta = " ++ show (1 / alfa1 - abs beta)
