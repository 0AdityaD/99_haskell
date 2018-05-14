import Data.List

-- Problem 54A

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Problem 55

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n = let (l, r) = quotRem (n - 1) 2 in
    [Branch 'x' left right | i <- [l .. l + r], left <- cbalTree i, right <- cbalTree (n - i - 1)]

-- Problem 56

mirror :: Tree a -> Tree a -> Bool
mirror (Empty) (Empty) = True
mirror (Branch _ t1 t2) (Branch _ t3 t4) = mirror t1 t4 && mirror t2 t3
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric t = mirror t t

-- Problem 57

halve :: [a] -> ([a], a, [a])
halve xs =
    let len = length xs in
    let q = div len 2 in
    (take q xs, xs !! q, drop (q + 1) xs)

construct :: Ord a => [a] -> Tree a
construct [] = Empty
construct xxs =
    let xs = sort xxs in
    let (xs1, x, xs2) = halve xs in
    (Branch x (construct xs1) (construct xs2))
