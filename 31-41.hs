-- Problem 31

isPrime :: Int -> Bool
isPrime x = and . map (\y -> mod x y > 0) . takeWhile (\y -> (y*y) < x) $ [2..]

-- Problem 32

gcd' :: Int -> Int -> Int
gcd' x 0 = x
gcd' a b = gcd b (mod a b)

-- Problem 33

coprime :: Int -> Int -> Bool
coprime x y = (gcd' x y) == 1

-- Problem 34

phi :: Int -> Int
phi x = length . filter (\y -> coprime x y) $ [1..x]

-- Problem 35

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [n | n <- xs, mod n x > 0]

primes :: [Int]
primes = sieve [2..]

primeFactorsHelper :: Int -> [Int] -> [Int]
primeFactorsHelper num (p:ps)
    | num <= 1            =   []
    | (mod num p) == 0    =   p : (primeFactorsHelper (div num p) (p:ps))
    | otherwise           =   primeFactorsHelper num ps

primeFactors :: Int -> [Int]
primeFactors num = primeFactorsHelper num primes 

-- Problem 36

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult num = reduce . primeFactors $ num
    where   reduce [] = []
            reduce (x:xs) = (x, count x (x:xs)) : (reduce (remove x xs))
            count x = length . filter (\y -> y == x)
            remove x = filter (\y -> y /= x)

-- Problem 37

phi' :: Int -> Int
phi' = foldr (*) 1 . map (\x -> let (a,b) = x in (a - 1) * a ^ (b - 1)) . primeFactorsMult

-- Problem 39

primesR :: Int -> Int -> [Int]
primesR a b = takeWhile (\x -> (x <= b)) . dropWhile (\x -> (x < a)) $ primes

-- Problem 40

goldbach :: Int -> Maybe (Int, Int)
goldbach n
    | length ls == 0    =   Nothing
    | otherwise         =   pure (head ls)
        where ls = [(x,y) | x <- primesR 0 n, y <- primesR 0 n, x + y == n]

-- Problem 41

goldbachList :: [Int] -> [Maybe (Int,Int)]
goldbachList = map goldbach 
