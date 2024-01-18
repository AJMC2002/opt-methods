module Main where

import Data.Massiv.Array as A
import Formst (showMat, showVec)
import Optimization.DirectTask qualified as DirectTask
import System.Random qualified as R
import Text.Printf (printf)
import Utils
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
        dim = 4
        tempA = computeP $ uniformRangeArray gen1 rng comp (Sz2 dim dim) :: Matrix P Double
        -- vecB = computeP $ uniformRangeArray gen2 rng comp (Sz1 dim)
        -- vecX0 = computeP $ uniformRangeArray gen3 rng comp (Sz dim)
        a =
            resize' @P (Sz2 8 6)
                $ fromList
                    (ParN 0)
                    [15 :: Double, 115, 106, 290, 232, 167, 79, 247, 7, 286, 65, 276, 219, 125, 174, 42, 114, 202, 287, 213, 225, 274, 169, 260, 202, 124, 211, 200, 174, 183, 158, 265, 1, 39, 113, 290, 175, 196, 170, 270, 187, 178, 245, 100, 226, 63, 245, 259]
        b = fromList @P (ParN 0) [296 :: Double, 85, 22, 47, 247, 28, 125, 218]
        c = fromList @P (ParN 0) [173 :: Double, 299, 240, 120, 249, 86]
    sol <- DirectTask.solve a b c
    putStrLn $ "\"Оптимальное решение\" = " ++ showVec (fst sol) 2
    putStrLn $ "\"Целевая функция\" = " ++ show (snd sol)
