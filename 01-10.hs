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
