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

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus = undefined

nmult :: N -> N -> N
nmult = undefined

nsub :: N -> N -> Maybe N
nsub = undefined

ncmp :: N -> N -> Ordering
ncmp = undefined

nFromNatural :: Natural -> N
nFromNatural = undefined

nToNum :: Num a => N -> a
nToNum = undefined

nEven :: N -> Bool
nEven = undefined

nOdd :: N -> Bool
nOdd = undefined

ndiv :: N -> N -> N
ndiv = undefined

nmod :: N -> N -> N
nmod = undefined
