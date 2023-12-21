module Main where

import Data.Massiv.Array as A
import Function (Functions (..))
import Minimization (Minimization (..), mkMinimization)
import System.Random qualified as R
import Utils (split)
import Prelude as P

main :: IO ()
main =
    let
        -- Initial values
        salt = 190902
        gen = R.mkStdGen salt
        (gen1, gen') = R.split gen
        (gen2, gen3) = R.split gen'
        rng = (-10 :: Double, 10)
        comp = ParN 0
        dim = 4
        tempA = computeP $ uniformRangeArray gen1 rng comp (Sz2 dim dim) :: Matrix P Double
        matA = (tempA !+! computeP (transpose tempA)) ./ 2 -- this generates a symmetric matrix
        vecB = computeP $ uniformRangeArray gen2 rng comp (Sz1 dim)
        vecX0 = computeP $ uniformRangeArray gen3 rng comp (Sz dim)
        r = 5
        epsilon = 1.0e-6
        minimization = mkMinimization matA vecB vecX0 r epsilon
        funs = getFunctions minimization
        -- Part 1 | When y = 0
        vecXSus = yIsZero minimization -- podozritel'niy
        fSus = f funs vecXSus
        distanceToCentre = normL2 (vecXSus !-! vecX0)
        isInSphere = distanceToCentre <= r
        -- Part 2 | When y > 0
        numPoints = 8
        gs = P.tail $ P.take (numPoints + 1) $ iterate (snd . R.split) gen3
        xy0s =
            P.map
                ( \g ->
                    let
                        xy' = computeP @P $ uniformRangeArray g rng comp (Sz1 dim + 1)
                        xy = makeArray @P (ParN 0) (Sz1 dim + 1) (\i -> if i == dim then abs (xy' ! i) else xy' ! i)
                     in
                        xy
                )
                gs
        xySols = P.map (split . yIsGreaterThanZero minimization) xy0s
        fXY = P.map (f funs . fst) xySols
     in
        do
            putStrLn "A"
            print matA
            putStrLn "b"
            print vecB
            putStrLn "x0"
            print vecX0
            putStrLn "r"
            print r
            putStrLn "y = 0"
            putStrLn "Solution"
            print vecXSus
            putStrLn "Minimal value"
            print fSus
            putStrLn "Distance to centre"
            print distanceToCentre
            putStrLn "Is in sphere?"
            print isInSphere
            putStrLn "y > 0"
            putStrLn "Initial vectors"
            print xy0s
            putStrLn "Solutions"
            print xySols
            putStrLn "Minimal values"
            print fXY
