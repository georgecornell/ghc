double x = x + x

quadrupl x = double(double x)

factorial n = product [1 .. n] 

average ns = sum ns `div` length ns

-- | the add function with function of functions
-- | this is a higher order function
-- Int -> (Int -> Int)
add1    :: Int -> Int -> Int
add1 x y = x+y

-- | the add function with tuples
add2      :: (Int, Int) -> Int
add2 (x,y) = x + y

signum2  :: Int -> Int
signum2 n = if n < 0 then -1 
            else if n==0 then 0 
            else 1

-- | using guarded condition
abs2  :: Int -> Int
abs2 n | n >= 0 = n 
       | otherwise = -n


signum1  :: Int -> Int
signum1 n | n < 0  = -1 
          | n==0   = 0 
          |  otherwise = 1

tail2       ::[a] -> [a]
tail2 (a:as) = as

-- | list comprehesion example
factors  :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime

-- | test for factorial
prime  :: Int -> Bool
prime n = factors n == [1,n]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | z <- [1..n], 
                     y <-[1..z], 
                     x <- [1..y-z], 
                          x^2+y^2 == z^2 ]

intSquareRoot :: Int -> Int
intSquareRoot n
    | n*n > n   = intSquareRoot (n - 1) 
    | n*n <= n  = n

pyths2 :: Int -> [(Int,Int,Int)]
pyths2 l = [(m^2-n^2, 2*m*n, m^2+n^2) | m <-[2..ceiling $ sqrt $ fromIntegral l], 
                                        n <- [1 .. m-1],
                                             m^2+n^2 <= l]

perfects    :: Int -> [Int]
perfects n = [x | x <- [1..n], sum(factor x) +1 == x]

qsort       :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where 
                  smaller = [a | a <- xs, a<=x]
                  larger  = [b | b <- xs, b >x]



