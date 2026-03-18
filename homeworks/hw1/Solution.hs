{-# LANGUAGE BangPatterns #-}
module Solution where
import Data.Function
import Data.List (transpose)

-- Task 1
--isPrime :: Int -> Bool
--isPrime n
-- | n <= 1 = False
-- | n == 2 = True
-- | even n = False
-- | otherwise = all (/= 0) [ n `mod` i | i <- [2..s]]
--  where
--    s = n & fromIntegral & sqrt & floor


goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
    | n <= 4 = []
    | otherwise = [ (x, y) | x <- primes, y <- primes, x + y == n, x <= y ]
    where
    primes = filter isPrime [2..n]


-- Task 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x:xs) = [ (x,y) | y <- xs, gcd x y == 1 ] ++ coprimePairs xs


-- Task 3
-- Assuming the input list is sorted and lowest value is 2
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [ r | r <- xs, r `mod` p /= 0 ]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n = primesTo n & elem n


-- Task 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = [[ sum (zipWith (*) row col) | col <- transpose' b ] | row <- a]
    where
    transpose' :: [[Int]] -> [[Int]]
    transpose' [] = []
    transpose' ([]:_) = []
    transpose' a = map head a : transpose' (map tail a)


-- Task 5
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs = [ y:ps | (y, ys) <- selections xs, ps <- permutations (k-1) ys ]
    where
    selections :: [a] -> [(a, [a])]
    selections [] = []
    selections (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- selections xs ]


-- Task 6
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x > y = y : merge (x:xs) ys
    | otherwise = x : merge xs ys

hamming :: [Int]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))


-- Task 7
power :: Int -> Int -> Int
power b e = go b e 1
    where
    go _ 0 !acc = acc
    go b e !acc = go b (e-1) (b*acc)


-- Task 8
-- Assuming the list is non-empty
listMax :: [Int] -> Int
listMax (x:xs) = go x xs
    where
    go max [] = max
    go max (x:xs) =
        let newMax = if x > max then x else max
        in go newMax xs

listMax' :: [Int] -> Int
listMax' (x:xs) = go x xs
    where
    go !max [] = max
    go !max (x:xs) = go (if x > max then x else max) xs


-- Task 9
primes :: [Int]
primes = sieve [2..]

isPrime' :: Int -> Bool
isPrime' n = primes & takeWhile (<= n) & elem n


-- Task 10
mean :: [Double] -> Double
mean [] = 0
mean xs = go xs 0 0
    where
    go [] sum count = sum /count
    go (x:xs) sum count = go xs (x + sum) (count + 1)

mean' :: [Double] -> Double
mean' [] = 0
mean' xs = go xs 0 0
    where
    go [] !sum !count = sum / count
    go (x:xs) !sum !count = go xs (x + sum) (count + 1)

meanAndVariance :: [Double] -> (Double, Double)
meanAndVariance [] = (0, 0)
meanAndVariance xs = go xs 0 0 0
    where
    go [] !sum !count !sumSq = (sum / count, sumSq / count - (sum / count) * (sum / count))
    go (x:xs) !sum !count !sumSq = go xs (x + sum) (count + 1) (x*x + sumSq)
