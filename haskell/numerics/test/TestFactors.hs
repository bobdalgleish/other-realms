module TestFactors where

import Factorization.Factors
import QuickCheck

positiveInt = let anInt = arbitrary
              in if anInt > 1 then anInt else _

prop_factorization n = (n > 0) ==> n == product $ allFactors n
    where
        types = n::Int