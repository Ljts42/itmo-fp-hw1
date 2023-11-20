module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = ()

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize = undefined

tdepth :: Tree a -> Int
tdepth = undefined

tmember :: Ord a => a -> Tree a -> Bool
tmember = undefined

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert = undefined

tFromList :: Ord a => [a] -> Tree a
tFromList = undefined
