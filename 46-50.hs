import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

-- Problem 46 and 47

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

table :: (Bool -> Bool -> Bool) -> IO ()
table f =
    let bools = [False, True] in
    let tab = [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- bools, b <- bools] in
    mapM_ putStrLn tab 

-- Problem 48

combs :: Int -> [[Bool]]
combs 0 = [[]]
combs n = map (++ [False]) (combs (n-1)) ++ map (++ [True]) (combs (n-1))

tableN :: Int -> ([Bool] -> Bool) -> IO ()
tableN n f =
    let choices = combs n in
    let tab = [c ++ [f c] | c <- choices] in
    let strtab = map (unwords . map show) tab in
    mapM_ putStrLn strtab

-- Problem 49

gray :: Int -> [String]
gray 0 = [""]
gray n = map ("0" ++) (gray (n-1)) ++ map ("1" ++) (reverse (gray (n-1)))

-- Problem 50

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

eval :: [(Int, Tree a)] -> Tree a
eval [(_,x)] = x
eval ((w1,t1):(w2,t2):wts) = eval . insertBy (comparing fst) (w1+w2,Node t1 t2) $ wts

serialize :: Tree a -> String -> [(a, String)]
serialize (Node t1 t2) prefix = serialize t1 (prefix ++ "0") ++ serialize t2 (prefix ++ "1")
serialize (Leaf c) prefix = [(c, prefix)]

huffman :: [(Char,Int)] -> [(Char, String)]
huffman freqs =
    let queue = sortBy (comparing fst) [(freq, Leaf c) | (c, freq) <- freqs] in
    let tree = eval queue in
    let ls = serialize tree "" in
    sortBy (comparing fst) ls
