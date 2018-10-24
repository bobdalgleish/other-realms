module Factors where

import Primes (primesSA)
import Data.List.Ordered (union)
import Data.List (nub)

-- |Provide a list of primes below 'n'
primesBelow :: Int -> [Int]
primesBelow n = takeWhile (<n) primesSA

-- |Return the factors of k, from the list of integers.
--  'k == product $ factorsFrom list k'
factorsFrom :: [Int] -> Int -> [Int]
factorsFrom [] _ = []
factorsFrom p@(p1:ps) k =
        let (kr, kq) = k `quotRem` p1
        in if kq == 0
            then p1: factorsFrom p kr
            else factorsFrom ps k

-- |Prime factors of k
primeFactors :: Int -> [Int]
primeFactors 2 = [2]
primeFactors k =
    let pf = primesBelow (k+1)
    in factorsFrom pf k

-- |Produce all unique factors of k
allFactors :: Int -> [Int]
allFactors k =
    let pf = primeFactors k
    in 1: thisFactor pf
    where
        thisFactor :: [Int] -> [Int]
        thisFactor [] = []
        thisFactor (p:ps) = let subFactors = thisFactor ps
                                pSubFactors = map (p*) subFactors
                            in nub $ union (union subFactors (p:subFactors)) pSubFactors