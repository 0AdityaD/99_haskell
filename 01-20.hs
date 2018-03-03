-- Problem 1

myLast :: [a] -> Maybe a
myLast = foldl (\x y -> x) Nothing . map Just

-- Problem 2

myButLast :: [a] -> Maybe a
myButLast []      = Nothing
myButLast [x]     = Nothing
myButLast (x:[y]) = Just x
myButLast (x:xs)  = myButLast xs

-- Problem 3

elementAt :: [a] -> Int -> Maybe a
elementAt (x:xs) 0 = Just x
elementAt (x:xs) i = elementAt xs (i-1)
elementAt []     _ = Nothing

-- Problem 4

myLength :: [a] -> Int
myLength = foldr (\x y -> y + 1) 0

-- Problem 5

myReverse :: [a] -> [a]
myReverse xs = (foldr (\x y -> y . (x:)) id xs) $ []

-- Problem 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (reverse x)

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' x = foldr (&&) True (zipWith (==) x (reverse x))

-- Problem 7

data NestedList a = Elem a | Cons (NestedList a) (NestedList a)
    deriving (Show, Eq)

flatten :: NestedList a -> [a]
flatten (Elem a)   = [a]
flatten (Cons x y) = flatten x ++ flatten y

-- Problem 8

compressHelper :: Eq a => a -> [a] -> [a]
compressHelper x [] = [x]
compressHelper x (y:ys)
    | x == y        = compressHelper x ys
    | otherwise     = x : (compressHelper y ys)

compress :: Eq a => [a] -> [a]
compress xs
    | length xs /= 0    = compressHelper x xs
    | otherwise         = []
        where x = head xs

-- Problem 9

pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x: takeWhile (f) xs) : pack (dropWhile (f) xs)
    where f y = x == y

-- Problem 10

encode :: Eq a => [a] -> [(Int, a)]
encode = map (f) . pack
    where f x = (length x, head x)

-- Problem 11

data Count a = Multiple Int a | Single a
    deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Count a]
encodeModified = map (f) . encode
    where   f (1,x) = Single x
            f (n,x) = Multiple n x

-- Problem 12

decodeModified :: Eq a => [Count a] -> [a]
decodeModified = concat . map (f)
    where   f (Single x) = [x]
            f (Multiple n x) = [x | _ <- [1..n]]

-- Problem 13

encodeDirect :: Eq a => [a] -> [Count a]
encodeDirect = map f . pack
    where   f [x] = Single x
            f xs = Multiple (length xs) (head xs)

-- Problem 14

dupli :: [a] -> [a]
dupli = foldr (\x y -> x:x:y) []

-- Problem 15

repli :: Int -> [a] -> [a]
repli n = concat . foldr (\x y -> [x | _ <- [1..n]] : y) []

-- Problem 16

dropEvery :: Int -> [a] -> [a]
dropEvery n xs
    | (length xs) < n   = xs
    | otherwise         = init (take n xs) ++ (dropEvery n (drop n xs))

-- Problem 17

split :: Int -> [a] -> ([a], [a])
split n xs = (take n xs, drop n xs)

-- Problem 18

slice :: Int -> Int -> [a] -> [a]
slice a b xs
    | b < a     = []
    | otherwise = take (b-a+1) (drop (a-1) xs)

-- Problem 19

rotate :: Int -> [a] -> [a]
rotate n xs = (drop x xs) ++ (take x xs)
    where x = mod n (length xs)

-- Problem 20

removeAt :: Int -> [a] -> [a]
removeAt _ []     = []
removeAt 1 (x:xs) = xs
removeAt n (x:xs) = x:(removeAt (n-1) xs)
