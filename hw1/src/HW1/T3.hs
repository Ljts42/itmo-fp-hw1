module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = Int

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf             = 0
tsize (Branch s _ _ _) = s

tdepth :: Tree a -> Int
tdepth Leaf             = 0
tdepth (Branch _ l _ r) = 1 + max (tdepth l) (tdepth r)

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf                         = False
tmember x (Branch _ l v r) | x == v    = True
                           | x < v     = tmember x l
                           | otherwise = tmember x r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf                                = mkBranch Leaf x Leaf
tinsert x branch@(Branch _ l v r) | x == v    = branch
                                  | x < v     = tinsert x l
                                  | otherwise = tinsert x r

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l v r = Branch (1 + tsize l + tsize r) l v r
