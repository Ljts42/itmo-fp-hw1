module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import           Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus Z     Z     = Z
nplus Z     n     = n
nplus n     Z     = n
nplus (S a) (S b) = S $ S $ nplus a b

nmult :: N -> N -> N
nmult Z     _     = Z
nmult _     Z     = Z
nmult (S a) (S b) = S $ nplus a $ nplus b $ nmult a b

nsub :: N -> N -> Maybe N
nsub Z     _     = Nothing
nsub n     Z     = Just n
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp Z     Z     = EQ
ncmp Z     _     = LT
ncmp _     Z     = GT
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural $ n - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool
nEven Z     = True
nEven (S n) = nOdd n

nOdd :: N -> Bool
nOdd Z     = False
nOdd (S n) = nEven n

ndiv :: N -> N -> N
ndiv _ Z = error "division by zero"
ndiv a b = case nsub a b of
  Nothing  -> Z
  (Just n) -> S $ ndiv n b

nmod :: N -> N -> N
nmod _ Z = error "division by zero"
nmod a b = case nsub a b of
  Nothing  -> a
  (Just n) -> nmod n b
