module MatrixGame where

import Data.Massiv.Array as A
import Prelude as P

lowStrategy :: Matrix P Double -> Double
lowStrategy a = maximum $ P.map (minimum' . (a <!)) [0 .. (j - 1)] where Sz2 _ j = size a

highStrategy :: Matrix P Double -> Double
highStrategy a = minimum $ P.map (maximum' . (a !>)) [0 .. (i - 1)] where Sz2 i _ = size a
