module Factorization.Primes (
    primesTMWE, primesSA
) where

{-
These functions were taken from:
    https://wiki.haskell.org/Prime_numbers

The algorithm uses recursive 'union' to merge infinite, ordered lists.
Because each list input to 'union' is ordered, the function needs
only to look at the head of each list to determine which element
to include in the result.
-}

import Data.List.Ordered (minus, union)
import Data.Array.Unboxed

wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

_Y :: (t -> t) -> t
_Y g = g (_Y g)

gapsW :: (Ord a, Num a) => a -> [a] -> [a] -> [a]
gapsW k (d:w) s@(c:cs) | k < c     = k: gapsW (k+d) w s
                       | otherwise = gapsW (k+d) w cs

hitsW :: (Ord a, Num a) => a -> [a] -> [a] -> [[a]]
hitsW k (d:w) s@(p:ps) | k < p     = hitsW (k+d) w s
                       | otherwise = scanl (\c d->c+p*d) (p*p) (d:w) : hitsW (k+d) w ps

joinT :: Ord a => [[a]] -> [a]
joinT ((x:xs):t) = x : union xs (joinT (pairs t))
    where pairs (xs:ys:t) = union xs ys : pairs t

-- |"High-performance" prime number finder, using lists
primesTMWE :: [Int]
primesTMWE = [2,3,5,7] ++ _Y ((11:) . tail .gapsW 11 wheel . joinT . hitsW 11 wheel)

-- |"High-performance" prime number finder, using backing array
primesSA :: [Int]
primesSA = 2 : oddprimes ()
    where
        oddprimes = (3 :) . sieve 3 [] . oddprimes
        sieve x fs (p:ps) = [i*2 + x | (i,True) <- assocs a]
                            ++ sieve (p*p) ((p,0) :
                                [(s, rem (y-q) s) | (s,y) <- fs]) ps
            where
                q = (p*p-x) `div` 2
                a :: UArray Int Bool
                a = accumArray (\ b c -> False) True (1,q-1)
                               [(i,()) | (s,y) <- fs, i <- [y+s,y+s+s..q]]
