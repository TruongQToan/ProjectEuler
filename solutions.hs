 {-# LANGUAGE UnicodeSyntax #-}

module Euler(euler1, fibs) where

import Control.Monad.State

-- Problem 1
modFilter :: Int -> Bool
modFilter x = mod x 3  == 0 || mod x 5 == 0

euler1 :: IO ()
euler1 = do 
    let r = sum $ filter modFilter $ [1..999]
    putStrLn $ "Result: " ++ show r

-- Problem 2
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

euler2 :: Integer
euler2 = foldr (+) 0 $ filter (\n → n `mod` 2 == 0) $ takeWhile (\n → n <= 4000000) fibs

-- Problem 3
prime_factors :: Integer->[Integer]
prime_factors n =
    case factors of
        [] → [n]
        _ → factors ++ prime_factors (n `div` (head factors))
    where factors = take 1 $ filter (\x -> n `mod` x == 0) [2..(n-1)]

euler3 :: Integer->Integer
euler3 n = last $ prime_factors n

--- Problem 4
palindrome:: Integer → Bool
palindrome x = reversal x == x

reversal :: Integer → Integer
reversal = go 0
    where go a 0 = a
          go a b = let (q, r) = b `quotRem` 10 in go (a*10+r) q

palindrome_product :: Integer → Integer → Integer → Integer → Integer
palindrome_product lo_a hi_a lo_b hi_b = go hi_a hi_b (lo_a*lo_b)
    where go a b max = if a == lo_a-1
                        then max
                        else if b == lo_b-1
                            then go (a-1) hi_b max
                            else let max' = if palindrome (a*b) && a*b > max then a*b else max
                                 in go a (b-1) max'

euler4 :: Integer
euler4 = palindrome_product 100 999 100 999
