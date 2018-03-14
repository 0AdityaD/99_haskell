import System.Random
import Data.List

-- Problem 21

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y xs     = y:xs
insertAt _ y []     = [y]
insertAt n y (x:xs) = x : (insertAt (n-1) y xs)

-- Problem 22

range :: Int -> Int -> [Int]
range a b = take (b-a+1) . iterate (+1) $ a

-- Problem 23

rndSelect :: [Int] -> Int -> IO [Int]
rndSelect xs 0 = return []
rndSelect xs n = do y <- randomRIO (l, h)
                    ys <- rndSelect xs (n-1)
                    return ((xs !! y):ys)
    where   l = 0
            h = length xs - 1

-- Problem 24

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = diffSelect' n [1..m]

diffSelect' :: Int -> [Int] -> IO [Int]
diffSelect' 0 xs = return []
diffSelect' _ [] = return []
diffSelect' n xs = do   r <-randomRIO (0, length xs - 1)
                        let ys = take (r) xs ++ drop (r+1) xs    
                        rs <- diffSelect' (n-1) ys
                        return ((xs !! r):rs)

-- Problem 25

rndPermu :: [a] -> IO [a]
rndPermu [] = return []
rndPermu (xs) = do  r <- randomRIO (0, length xs - 1)
                    let ys = take (r) xs ++ drop (r+1) xs
                    rs <- rndPermu ys
                    return ((xs !! r):rs)

-- Problem 26

combinations :: Int -> [a] -> [[a]]
combinations 0 xs = [[]]
combinations n xs = [(xs !! i):ys | i <- [0..(length xs - 1)],
                                    ys <- combinations (n-1) (drop (i+1) xs)]
    
-- Problem 27

dropR :: Eq a => [a] -> [a] -> [a]
dropR [] xs = xs
dropR (i:is) xs = dropR is (delete i xs)

groupC :: Eq a => [Int] -> [a] -> [[[a]]]
groupC [] xs = [[]]
groupC (i:is) xs = [(s:g) | s <- selected, g <- (groupC is (dropR s xs))]
    where   selected = combinations i xs

-- Problem 28

lfsort :: Ord a => [[a]] -> [[a]]
lfsort = map(\x -> let (a, b) = x in b) . sort . map (\x -> (length x, x))
