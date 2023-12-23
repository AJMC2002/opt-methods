module Main where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Massiv.Array as A
import Function (Functions (..))
import Minimization (Minimization (..), mkMinimization)
import System.IO
import System.Random qualified as R
import Utils (split)
import Prelude as P

main :: IO ()
main =
    let
        -- Initial values
        salt = 190902
        gen1 = R.mkStdGen salt
        gen2 = snd $ R.split gen1
        gen3 = snd $ R.split gen2
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
        xySols = parMap rpar (split . yIsGreaterThanZero minimization) xy0s
        fXY = parMap rpar (f funs . fst) xySols
     in
        do
            handle <- openFile "output.txt" WriteMode
            hPutStrLn handle "A"
            hPrint handle matA
            hPutStrLn handle "b"
            hPrint handle vecB
            hPutStrLn handle "x0"
            hPrint handle vecX0
            hPutStrLn handle "r"
            hPrint handle r
            hPutStrLn handle "** y = 0 **"
            hPutStrLn handle "Solution"
            hPrint handle vecXSus
            hPutStrLn handle "Minimal value"
            hPrint handle fSus
            hPutStrLn handle "Distance to centre"
            hPrint handle distanceToCentre
            hPutStrLn handle "Is in sphere?"
            hPrint handle isInSphere
            hPutStrLn handle "** y > 0 **"
            hPutStrLn handle "Initial vectors"
            hPrint handle xy0s
            hPutStrLn handle "Solutions"
            hPrint handle xySols
            hPutStrLn handle "Minimal values"
            hPrint handle fXY
            hClose handle
