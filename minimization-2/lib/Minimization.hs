module Minimization (Minimization (..), mkMinimization) where

import Data.Massiv.Array as A
import Debug.Trace (trace)
import Function (Functions (..), mkFunctions)
import Utils (identity, inverse, split, zeros)
import Prelude as P

data Minimization r e = Minimization
    { getFunctions :: Functions r e
    , yIsZero :: Vector r e
    , yIsGreaterThanZero :: Vector r e -> Vector r e
    }

mkMinimization ::
    ( NumericFloat r e
    , Manifest r e
    , Load r Ix1 e
    , Load r Ix2 e
    , Ord e
    , Prim e
    ) =>
    Matrix r e ->
    Vector r e ->
    Vector r e ->
    e ->
    e ->
    Minimization r e
mkMinimization matA vecB vecX0 r epsilon =
    Minimization
        { getFunctions = functions
        , yIsZero = yIsZero' n functions
        , yIsGreaterThanZero = yIsGreaterThanZero' n matA vecX0 epsilon functions
        }
  where
    Sz2 n _ = size matA
    functions = mkFunctions matA vecB vecX0 r

yIsZero' :: (NumericFloat r e, Load r Ix1 e) => Int -> Functions r e -> Vector r e
yIsZero' n functions = fPrimeInv functions $ zeros $ Sz1 n

yIsGreaterThanZero' ::
    forall r e.
    ( NumericFloat r e
    , Manifest r e
    , Load r Ix1 e
    , Load r Ix2 e
    , Prim e
    , Ord e
    ) =>
    Int ->
    Matrix r e ->
    Vector r e ->
    e ->
    Functions r e ->
    Vector r e ->
    Vector r e
yIsGreaterThanZero' n matA vecX0 epsilon functions vecXk0 = computeP $ recur vecXk0 (0 :: Int)
  where
    recur :: Vector r e -> Int -> Vector r e
    recur xk k
        | k >= 10000 || normL2 (xkNext !-! xk) <= epsilon = xk
        | otherwise = recur xkNext (k + 1)
      where
        (x, y) = split xk
        xkNext = xk !-! computeP (inverse fPrimeXk !>< fXk)
        fXk = computeP @P $ concat' 1 [up, down]
          where
            up = lagrangePrimeX functions x y
            down = singleton $ g functions x
        fPrimeXk = computeP @P $ concat' 2 [up, down]
          where
            up = computeP @P $ concat' 1 [upL, upR]
            down = computeP @P $ concat' 1 [downL, downR]
            upL = matA !+! ((2 * y) *. identity n)
            upR = computeP $ resize' (Sz2 n 1) (2.0 *. (x !-! vecX0))
            downL = transpose upR
            downR = singleton 0
