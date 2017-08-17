module HW1
  (
  toDigits,
  toDigitsRev,
  doubleEveryOther,
  sumDigits,
  validate,
  )where

--exercise 1
toDigits:: Integer -> [Integer]
toDigits num
  |num <= 0 = []
  |otherwise = toDigits (num `div` 10) ++ (mod x 10)


toDigitsRev:: Integer -> [Integer]
toDigitsRev num =
  reverse (toDigits num)


--exercise 2
doubleEveryOther:: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther s@(x:xs)
  | (length s) `mod` 2 == 0 = (x*2) : (doubleEveryOther xs)
  | otherwise = x : (doubleEveryOther xs)


--exercise 3
sumDigits:: [Integer] -> Integer
sumDigits [] = 0
sumDigits (num:[])
  | num < 1 = 0
  | otherwise = num
sumDigits (num:nums) =
  num + (sumDigits nums)


-- not sure if I am doing this correctly. First I am splitting the digits
-- then I am adding them all up? But I am not sure if this order of operations
--will do what I think it is doing? What is easy way to check
-- in Haskell & run code??

--exercise 4
validate:: Integer -> Bool
validate num =
  num >= 0 && (mod (sumDigits(doubleEveryOther (toDigits num))) 10) == 0

--type Peg = String
--type Move = (Peg, Peg)

-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi n peg1 peg2 peg3 = []
