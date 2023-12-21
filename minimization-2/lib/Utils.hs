module Utils (det, inverse, zeros, identity, split) where

import Data.Massiv.Array as A
import Prelude as P

submatrix :: (Manifest r e, Load r Ix2 e, Size r) => Matrix r e -> Ix2 -> Matrix r e
submatrix m (Ix2 i j) = makeArray (ParN 0) (Sz2 (rows - 1) (cols - 1)) checkIx
  where
    Sz2 rows cols = size m
    checkIx (ixI :. ixJ) = m ! (newI :. newJ)
      where
        newI = if ixI >= i then ixI + 1 else ixI
        newJ = if ixJ >= j then ixJ + 1 else ixJ

minor :: (Manifest r e, Load r Ix2 e, Size r, Num e) => Matrix r e -> Ix2 -> e
minor m = det . submatrix m

cofactor :: (Manifest r e, Load r Ix2 e, Size r, Num e) => Matrix r e -> Ix2 -> e
cofactor m (Ix2 row col) = sign * minor m (Ix2 row col)
  where
    sign = if even (row + col) then 1 else -1

det :: (Manifest r e, Load r Ix2 e, Size r, Num e) => Matrix r e -> e
det m =
    let Sz2 rows cols = size m
     in if rows /= cols || rows < 1
            then error "Can't get the determinant of this matrix"
            else
                if rows == 1
                    then m ! Ix2 0 0
                    else P.sum $ P.map (\col -> (m ! Ix2 0 col) * cofactor m (Ix2 0 col)) [0 .. (cols - 1)]

inverse :: (Manifest r e, Load r Ix2 e, NumericFloat r e, Eq e) => Matrix r e -> Matrix r e
inverse m = case det m of
    0 -> error "Can't get the inverse of this matrix"
    d -> makeArray (ParN 0) (size m) (\(Ix2 i j) -> cofactor m (Ix2 j i)) ./ d

-----------------------------------------------------------

zeros :: (Numeric r e, Load r ix e) => Sz ix -> Array r ix e
zeros sz = A.replicate (ParN 0) sz 0

identity :: (Num e, Load r Ix2 e) => Int -> Matrix r e
identity n = makeArray (ParN 0) (Sz2 n n) (\(i :. j) -> if i == j then 1 else 0)

split :: (Size r, Source r e) => Vector r e -> (Vector r e, e)
split xk = (x, y)
  where
    x = slice' 0 (size xk - 1) xk
    y = last' xk
