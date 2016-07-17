{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Data.Char

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = fromIntegral $ digitToInt $ last $ show n

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n
 | n < 10    = 0
 | otherwise = read (init $ show n)::Integer

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
 | n <= 0    = []
 | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther d = case d of
 []             -> []
 (a : [])   -> [ a ]
 (a : b : rest) -> a : (2 * b) : doubleEveryOther(rest)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits digits = case digits of
 []             -> 0
 (h : t)  -> (foldl1 (+) (toRevDigits(h))) + (sumDigits t)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = undefined

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
