module Optimization.DualTask (solveHelper, solveDual) where

import Data.Functor ((<&>))
import Data.Massiv.Array as A
import Formst
import Optimization.DirectTask
import Utils (identity, zeros)
import Prelude as P

solveDual :: Matrix P Double -> Vector P Double -> Vector P Double -> IO (Vector P Double, Double)
solveDual a b c = do
    let Sz2 m n = size a
    (prev_simplex, pivots) <- solveHelper a c
    putStrLn "Двойственная задача"
    let !simplex' =
            computeP @P
                $ concat'
                    2
                    [ computeP @P
                        $ concat'
                            1
                            [ resize' (Sz2 1 m) b
                            , zeros (Sz2 1 (n + 1))
                            ]
                    , computeP @P
                        $ concat'
                            1
                            [ extract' (1 :. 0) (Sz2 n (m + n)) prev_simplex
                            , extract' (1 :. (m + 2 * n)) (Sz2 n 1) prev_simplex
                            ]
                    ]
    putStrLn $ "Pre-simplex: " ++ showMat simplex' 2
    let !simplex =
            foldl
                ( \accum j ->
                    ifBasisDo
                        accum
                        j
                        ( \i ->
                            makeArray @P
                                (ParN 0)
                                (size accum)
                                ( \(i' :. j') ->
                                    if i' == 0 && j' == j
                                        then 0
                                        else
                                            if i' == 0
                                                then (accum ! (0 :. j')) - (accum ! (0 :. j)) * (accum ! ((i + 1) :. j'))
                                                else accum ! (i' :. j')
                                )
                        , accum
                        )
                )
                simplex'
                [0 .. (n + m - 1)]
    putStrLn $ showMat simplex 2

    getNextSimplex (simplex, pivots) <&> getOptSol (size $ A.transpose a)

solveHelper :: Matrix P Double -> Vector P Double -> IO (Matrix P Double, [(Int, Int, Double)])
solveHelper a c = do
    putStrLn "Вспомогательная задача"
    putStrLn $ "Pre-simplex: " ++ showMat simplex' 2
    putStrLn $ showMat simplex 2
    (finalSimplex, pivots) <- getNextSimplex (simplex, [])
    let (!optSol, _) = getOptSol (size $ A.transpose a) (finalSimplex, pivots)
    putStrLn $ "\"Оптимальное решение\" = " ++ showVec optSol 2 ++ "\n"
    return (finalSimplex, pivots)
  where
    Sz2 m n = size a
    !simplex' =
        computeP @P
            $ concat'
                2
                [ computeP @P
                    $ concat'
                        1
                        [ zeros $ Sz2 1 (m + n)
                        , 1 +. (zeros @P $ Sz2 1 n)
                        , zeros $ Sz2 1 1 -- 0 vec | 1 vec | 0
                        ]
                , computeP @P
                    $ concat'
                        1
                        [ computeP @P $ A.transpose a
                        , (-1) *. identity n
                        , identity n
                        , resize' (Sz2 n 1) c -- aT | -identity | identity | c
                        ]
                ]
    !simplex =
        computeP @P
            $ concat'
                2
                [ resize' (Sz2 1 (m + 2 * n + 1))
                    $ fromList (ParN 0)
                    $ P.map
                        ( \j ->
                            (simplex' ! (0 :. j)) - P.sum (P.drop 1 $ toList (simplex' <! j))
                        )
                        [0 .. (m + 2 * n)]
                , computeP @P $ concat' 1 [computeP @P $ A.transpose a, (-1) *. identity n, identity n, resize' (Sz2 n 1) c]
                ]
