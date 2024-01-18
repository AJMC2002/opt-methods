module Formst (showMat, showVec) where

import Data.Massiv.Array
import Text.Printf (printf)

showMat :: Matrix P Double -> Int -> String
showMat mat k = "mat(\n" ++ genStr 0 0 "" ++ ")"
  where
    Sz2 m n = size mat
    addElem i j s = s ++ printf ("%." ++ show k ++ "f") (mat ! Ix2 i j)
    genStr !i !j !s
        | i == m - 1 && j == n - 1 = addElem i j s ++ "\n"
        | j == n - 1 = genStr (i + 1) 0 (addElem i j s ++ ";\n")
        | otherwise = genStr i (j + 1) (addElem i j s ++ ", ")

showVec :: Vector P Double -> Int -> String
showVec vec k = "vec( " ++ genStr 0 "" ++ " )"
  where
    Sz1 n = size vec
    addElem i s = s ++ printf ("%." ++ show k ++ "f") (vec ! i)
    genStr !i !s
        | i == n - 1 = addElem i s
        | otherwise = genStr (i + 1) (addElem i s ++ ", ")
