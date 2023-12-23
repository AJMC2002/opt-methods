module Function (Functions (..), mkFunctions) where

import Control.Parallel.Strategies (NFData)
import Data.Massiv.Array as A
import Utils
import Prelude as P

data Functions r e = Functions
    { f :: Vector r e -> e
    , fPrime :: Vector r e -> Vector r e
    , fPrimeInv :: Vector r e -> Vector r e
    , g :: Vector r e -> e
    , gPrime :: Vector r e -> Vector r e
    , lagrange :: Vector r e -> e -> e
    , lagrangePrimeX :: Vector r e -> e -> Vector r e
    }

mkFunctions ::
    ( Manifest r e
    , NumericFloat r e
    , Load r Ix2 e
    , Eq e
    , NFData e
    ) =>
    Matrix r e ->
    Vector r e ->
    Vector r e ->
    e ->
    Functions r e
mkFunctions matA vecB vecX0 r =
    Functions
        { f = f' matA vecB
        , fPrime = fPrime' matA vecB
        , fPrimeInv = fPrimeInv' matA vecB
        , g = g' r vecX0
        , gPrime = gPrime' vecX0
        , lagrange = lagrange' matA vecB vecX0 r
        , lagrangePrimeX = lagrangePrimeX' matA vecB vecX0
        }

f' :: (NumericFloat r e, Manifest r e) => Matrix r e -> Vector r e -> Vector r e -> e
f' matA vecB vecX = 0.5 * ((vecX ><! matA) !.! vecX) + vecB !.! vecX

fPrime' :: (NumericFloat r e, Manifest r e) => Matrix r e -> Vector r e -> Vector r e -> Vector r e
fPrime' matA vecB vecX = compute (matA !>< vecX) !+! vecB

fPrimeInv' :: (Manifest r e, NumericFloat r e, Eq e, Load r Ix2 e, NFData e) => Matrix r e -> Vector r e -> Vector r e -> Vector r e
fPrimeInv' matA vecB vecFX = compute $ inverse matA !>< (vecFX !-! vecB)

g' :: (FoldNumeric r e, Source r e, NumericFloat r e) => e -> Vector r e -> Vector r e -> e
g' r vecX0 vecX = normL2 (vecX !-! vecX0) ^ (2 :: Integer) - r ^ (2 :: Integer)

gPrime' :: (NumericFloat r e) => Vector r e -> Vector r e -> Vector r e
gPrime' vecX0 vecX = 2.0 *. (vecX !-! vecX0)

lagrange' :: (NumericFloat r e, Manifest r e) => Matrix r e -> Vector r e -> Vector r e -> e -> Vector r e -> e -> e
lagrange' matA vecB vecX0 r vecX y = f' matA vecB vecX + y * g' r vecX0 vecX

lagrangePrimeX' :: (NumericFloat r e, Manifest r e) => Matrix r e -> Vector r e -> Vector r e -> Vector r e -> e -> Vector r e
lagrangePrimeX' matA vecB vecX0 vecX y = fPrime' matA vecB vecX !+! y *. gPrime' vecX0 vecX
