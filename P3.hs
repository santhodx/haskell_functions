-- Name: Devdutt Santhosh
--

module P3 where

-- A list of all factors of n.
factors :: Integral a => a -> [a]
factors n = factorsHelper n n []
factorsHelper i n acc
  | i < 1          = acc
  | n `mod` i == 0 = factorsHelper (i - 1) n (i : acc)
  | otherwise      = factorsHelper (i - 1) n acc

-- True if n is prime.
isPrime :: Integral a => a -> Bool
isPrime n
  | n < 2     = False
  | otherwise = isPrimeHelper 2 n
isPrimeHelper i n
  | i * i > n      = True
  | n `mod` i == 0 = False
  | otherwise      = isPrimeHelper (i + 1) n

-- A list of all prime factors of n.
primeFactors :: Integral a => a -> [a]
primeFactors n = [i | i <- factors n, isPrime i]

-- A list of primes up to n.
primesUpTo :: Integral a => a -> [a]
primesUpTo n = [i | i <- [1..n], isPrime i]

-- True if n is a perfect number.
-- A number n is perfect if the sum of its factors is 2*n.
isPerfect :: Integral a => a -> Bool
isPerfect n
  | n < 1     = False
  | otherwise = (sum (factors n)) == (2 * n)

-- A list of all perfect numbers up to n.
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n = [i | i <- [1..n], isPerfect i]

-- The next prime greater than n.
nextPrime :: Integral a => a -> a
nextPrime n
  | n < 2     = 2
  | isPrime m = m
  | otherwise = nextPrime m
  where m = n + 1

-- A list of the first n primes.
generatePrimes :: Integral a => a -> [a]
generatePrimes n 
  | n < 1 = []
  | otherwise = take (fromIntegral n) [i | i <- [2..], isPrime i]

