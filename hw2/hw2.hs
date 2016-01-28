------------------------------------------------------------------------------
-- Authors: Coy Humphrey (cmhumphr@ucsc.edu)
-- Program: hw2.hs
-- NOTE   : sayNum is implemented in Haskell at the bottom of this file
------------------------------------------------------------------------------

-- First call: base op x, second: (base op x) op x ...
-- Base case returns accumulated result up the chain
myFoldl :: (a->b->a) -> a -> [b] -> a
myFoldl _ base [] = base
myFoldl op base (x:xs) = myFoldl op (base `op` x) xs


-- Go through list in order appending each element to front of new list
-- Same principle as reversal using a stack
myReverse :: [a] -> [a]
myReverse = foldl op []
          where op x y = y:x


-- Start from right of list by using a reversed list
-- Flip order of op to fit foldl type and to counter the reversed list
myFoldr :: (a->b->b) -> b -> [a] -> b
myFoldr op base xs = foldl op' base (reverse xs)
                   where op' x y = y `op` x


-- Same as above, but the opposite.
myFoldl2 :: (a->b->a) -> a -> [b] -> a
myFoldl2 op base xs = foldr op' base (reverse xs)
                    where op' x y = y `op` x


isUpper :: Char -> Bool
isUpper c = c `elem` ['A'..'Z']


onlyCapitals1 :: String -> String
onlyCapitals1 = filter isUpper


onlyCapitals2 :: String -> String
onlyCapitals2 s = [c | c <- s, isUpper c]


onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (c:cs)
              | isUpper c = c : onlyCapitals3 cs
              | otherwise = onlyCapitals3 cs


divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = (x `div` y, x `mod` y)


digitSum :: Int -> Int
digitSum n
         | n == 0 = 0
         | otherwise = rem + digitSum qo
         where (qo, rem) = divRemainder n 10


------------------------------------------------------------------------------
-- sayNum code found below
------------------------------------------------------------------------------

oneDigit = 
	["","one ","two ","three ","four ","five ","six ","seven ","eight ",
	 "nine ","ten ","eleven ","twelve ","thirteen ","fourteen ","fifteen ",
	 "sixteen ", "seventeen ", "eighteen ", "nineteen "]

twoDigit = 
	["", "", "twenty ", "thirty ", "fourty ", "fifty ", "sixty ", "seventy ",
     "eighty ", "ninety "]

numNames = 
	["", "thousand", "million", "billion", "trillion", "quadrillion",
     "quintillion", "sextillion", "septillion", "octillion", "nonillion",
     "decillion", "undecillion", "duodecillion", "tredecillion", 
     "quattuordecillion", "quindecillion", "sexdecillion", "septendecillion",
     "octodecillion", "novemdecillion", "vigintillion"]


-- Given a string of digits (optionally starting with a -) returns an english
-- representation of the number.
-- Supports negative numbers and zero just for fun.
sayNum :: String -> String
sayNum s
       | num == 0  = "zero"
       | num < 0 = "negative " ++ sayNum (tail s)
       | otherwise = unwords (reverse (sayNum' (thousandChunks s) numNames))
       where num = read s :: Integer


-- Given a list of Strings containing numbers with 3 digits or fewer, and a
-- list of suffixes ("", "thousand", "million" ..) returns a list of Strings
-- containing an English representation of the 3 digit number followed by the
-- suffix
-- Example: sayNum' ["123", "456"] ["", "Thousand"] = 
-- ["one hundred twenty three", "four hundred fifty six thousand"]
sayNum' :: [String] -> [String] -> [String]
sayNum' [] _ = []
sayNum' _ [] = []
sayNum' (c:chunks) (n:nums) 
        | num == 0 = sayNum' chunks nums
        | otherwise = (say3DigNum num ++ n) : sayNum' chunks nums
        where num = read c :: Int


-- Reverses the String and breaks it up into groups of threes
thousandChunks :: String -> [String]
thousandChunks s = [reverse x | x <- splitToThrees (reverse s)]


-- Breaks the String up into threes
splitToThrees :: String -> [String]
splitToThrees [] = []
splitToThrees s = take 3 s : splitToThrees (drop 3 s)


-- Returns an English representation of the three digit number
-- An Int is used because the number is guaranteed to fall between 0 and 1000
say3DigNum :: Int -> String
say3DigNum n
           | n > 99 = (oneDigit !! hundreds) ++ "hundred " ++ say2DigNum rem
           | otherwise = say2DigNum n
           where (hundreds, rem) = divRemainder n 100


-- Returns an English representation of the two digit number
-- An Int is used because the number is guaranteed to fall between 0 and 100
say2DigNum :: Int -> String
say2DigNum n
           | n > 19 = (twoDigit !! qo) ++ (oneDigit !! rem)
           | otherwise = oneDigit !! n
           where (qo, rem) = divRemainder n 10
