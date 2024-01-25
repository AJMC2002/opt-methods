module Optimization.DirectTask (solveDirect, getNextSimplex, getOptSol, ifBasisDo, isBasis) where

import Control.Monad (guard, (>=>))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (elemIndex, minimumBy)
import Data.Massiv.Array as A
import Formst
import Utils (identity, zeros)
import Prelude as P

solveDirect :: Matrix P Double -> Vector P Double -> Vector P Double -> IO (Vector P Double, Double)
solveDirect a b c = do
    putStrLn "Прямая задача"
    putStrLn $ showMat simplex 2
    getNextSimplex (simplex, []) <&> getOptSol (size a)
  where
    Sz2 m n = size a
    simplex =
        computeP @P
            $ concat'
                2
                [ computeP @P $ concat' 1 [resize' (Sz2 1 n) ((-1) *. c), zeros $ Sz2 1 (m + 1)] -- c | 0 vec | 0
                , computeP @P $ concat' 1 [a, identity m, resize' (Sz2 m 1) b] -- a | identity | b
                ]

getNextSimplex :: (Matrix P Double, [(Int, Int, Double)]) -> IO (Matrix P Double, [(Int, Int, Double)])
getNextSimplex mat_pivots = maybe (return mat_pivots) (printSimplexData >=> getNextSimplex) (updateSimplex mat_pivots)

updateSimplex :: (Matrix P Double, [(Int, Int, Double)]) -> Maybe (Matrix P Double, [(Int, Int, Double)])
updateSimplex (simplex, pivots) = do
    let Sz2 m n = size simplex

    let c =
            filter ((0 >) . snd)
                $ P.take (n - 1)
                $ P.zip [0 ..]
                $ toList
                $ simplex
                !> 0
    guard $ not $ null c
    let pivot_col = fst $ minimumBy (compare `on` snd) c

    let b_ratios =
            filter ((0 <=) . snd)
                $ P.drop 1
                $ P.zip [0 ..]
                $ toList
                    ( (simplex <! (n - 1))
                        !/! (simplex <! pivot_col)
                    )
    guard $ not $ null b_ratios
    let pivot_row = fst $ minimumBy (compare `on` snd) b_ratios

    let pivot_val = simplex ! (pivot_row :. pivot_col)

    let new_simplex =
            makeArray
                (ParN 0)
                (size simplex)
                ( \(i :. j) ->
                    let
                        val = simplex ! (i :. j)
                     in
                        if i == pivot_row && j == pivot_col
                            then 1
                            else
                                if i == pivot_row
                                    then val / pivot_val
                                    else
                                        if j == pivot_col
                                            then 0
                                            else val - (simplex ! (i :. pivot_col)) * (simplex ! (pivot_row :. j)) / pivot_val
                )
        new_pivots = pivots ++ [(pivot_row, pivot_col, pivot_val)]

    Just
        ( new_simplex
        , new_pivots
        )

printSimplexData :: (Matrix P Double, [(Int, Int, Double)]) -> IO (Matrix P Double, [(Int, Int, Double)])
printSimplexData
    (simplex, pivots) = do
        let (row, col, val) = last pivots
        putStrLn $ "\"разрешающая строка\" = \"№\"" ++ show (row + 1)
        putStrLn $ "\"разрешающий столбец\" = \"№\"" ++ show (col + 1)
        putStrLn $ "\"разрешающий элемент\" = " ++ show val
        putStrLn $ showMat simplex 2
        return (simplex, pivots)

getOptSol :: Sz2 -> (Matrix P Double, [(Int, Int, Double)]) -> (Vector P Double, Double)
getOptSol (Sz2 _ a_cols) (simplex, pivots) =
    let Sz2 _ simplex_cols = size simplex
        updateOptSol sol [] = sol
        updateOptSol sol ((i, j, _) : ps) =
            updateOptSol
                ( if j < a_cols && (simplex ! (i :. j)) == 1
                    then replaceVal sol j (simplex ! (i :. (simplex_cols - 1)))
                    else sol
                )
                ps
        replaceVal list i newVal =
            l ++ [newVal] ++ r
          where
            (l, r') = P.splitAt i list
            r = if null r' then r' else P.tail r'
     in ( fromList (ParN 0)
            $ updateOptSol (P.replicate a_cols 0) pivots
        , abs $ simplex ! (0 :. (simplex_cols - 1))
        )

-- DONT FORGET TO ADD THE +1 TO THE ROW INDEX
ifBasisDo :: Matrix P Double -> Int -> (Int -> a, a) -> a
ifBasisDo mat j (case_just, case_nothing) =
    let column = P.drop 1 $ toList (mat <! j)
     in if P.sum column == 1
            then maybe case_nothing case_just (elemIndex 1 column)
            else case_nothing

isBasis mat j = ifBasisDo mat j (\i -> (mat ! ((i + 1) :. j)) == 1, False)
