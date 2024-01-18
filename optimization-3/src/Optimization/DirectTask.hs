module Optimization.DirectTask (solve) where

import Control.Monad (guard, (>=>))
import Data.Function (on)
import Data.List (elemIndex, minimumBy)
import Data.Massiv.Array as A
import Debug.Trace (trace)
import Formst
import Utils (identity, zeros)
import Prelude as P

solve :: Matrix P Double -> Vector P Double -> Vector P Double -> IO (Vector P Double, Double)
solve a b c = solve' (trace (showMat simplex 2) simplex) >>= getSol
  where
    Sz2 m n = size a
    simplex =
        computeP @P
            $ concat'
                2
                [ computeP @P $ concat' 1 [resize' (Sz2 1 n) ((-1) *. c), zeros $ Sz2 1 (m + 1)] -- c | 0 vec | 0
                , computeP @P $ concat' 1 [a, identity m, resize' (Sz2 m 1) b] -- a | identity | b
                ]
    solve' !mat = maybe (return mat) (printInfo >=> solve') (updateMat mat)
    printInfo mat_data = do
        let (mat, (row, col, val)) = mat_data
        putStrLn $ "\"разрешающий столбец\" = \"№\"" ++ show (row + 1)
        putStrLn $ "\"разрешающая строка\" = \"№\"" ++ show (col + 1)
        putStrLn $ "\"разрешающий элемент\" = " ++ show val
        putStrLn $ showMat mat 2
        return mat
    getSol mat =
        return
            ( fromList (ParN 0)
                $ P.map
                    ( \i -> case elemIndex 1 $ P.drop 1 $ toList (mat <! i) of
                        Just idx -> mat ! ((idx + 1) :. (m + n))
                        Nothing -> 0
                    )
                    [0 .. (n - 1)]
            , mat ! (0 :. (m + n))
            )

updateMat :: Matrix P Double -> Maybe (Matrix P Double, (Int, Int, Double))
updateMat mat = do
    let Sz2 m n = size mat

    let c =
            filter ((>) 0 . snd)
                $ P.take (n - m)
                $ P.zip [0 ..]
                $ toList
                $ mat
                !> 0
    guard $ not $ null c
    let pivot_col = fst $ minimumBy (compare `on` snd) c

    let b_ratios =
            filter ((<=) 0 . snd)
                $ P.drop 1
                $ P.zip [0 ..]
                $ toList
                $ (mat <! n - 1)
                !/! (mat <! pivot_col)
    guard $ not $ null b_ratios
    let pivot_row = fst $ minimumBy (compare `on` snd) b_ratios

    Just
        ( makeArray
            (ParN 0)
            (size mat)
            ( \(i :. j) ->
                let
                    val = mat ! (i :. j)
                    pivot_val = mat ! (pivot_row :. pivot_col)
                 in
                    if i == pivot_row
                        then val / pivot_val
                        else val - (mat ! (i :. pivot_col)) * (mat ! (pivot_row :. j)) / pivot_val
            )
        , (pivot_row, pivot_col, mat ! (pivot_row :. pivot_col))
        )
