module Main where

import Data.Massiv.Array as A
import Formst
import Optimization.DirectTask qualified as DirectTask
import Optimization.DualTask qualified as DualTask
import System.Random qualified as R
import Prelude as P

main :: IO ()
main = do
    let
        -- Initial values
        salt = 190902
        gen1 = R.mkStdGen salt
        gen2 = snd $ R.split gen1
        gen3 = snd $ R.split gen2
        rng = (0 :: Double, 100)
        comp = ParN 0
        a = computeP $ uniformRangeArray gen1 rng comp (Sz2 8 6) :: Matrix P Double
        b = computeP $ uniformRangeArray gen2 rng comp (Sz1 8)
        c = computeP $ uniformRangeArray gen3 rng comp (Sz 6)
    sol <- DirectTask.solveDirect a b c
    putStrLn $ "\"Оптимальное решение\" = " ++ showVec (fst sol) 2
    putStrLn $ "\"Целевая функция\" = " ++ show (snd sol) ++ "\n"
    sol2 <- DualTask.solveDual a b c
    putStrLn $ "\"Оптимальное решение\" = " ++ showVec (fst sol2) 2
    putStrLn $ "\"Целевая функция\" = " ++ show (snd sol2)
