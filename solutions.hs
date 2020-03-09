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


