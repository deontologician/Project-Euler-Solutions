module Q17 where

{- 

Question 17: 

If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters. The use of "and" when writing out numbers is in compliance with British
usage.  

-}

intToWord :: Int -> String
intToWord 1 = "one"
intToWord 2 = "two"
intToWord 3 = "three"
intToWord 4 = "four"
intToWord 5 = "five"
intToWord 6 = "six"
intToWord 7 = "seven"
intToWord 8 = "eight"
intToWord 9 = "nine"
intToWord 10 = "ten"
intToWord 11 = "eleven"
intToWord 12 = "twelve"
intToWord 13 = "thirteen"
intToWord 14 = "fourteen"
intToWord 15 = "fifteen"
intToWord 16 = "sixteen"
intToWord 17 = "seventeen"
intToWord 18 = "eighteen"
intToWord 19 = "nineteen"
intToWord 20 = "twenty"
intToWord 30 = "thirty"
intToWord 40 = "forty"
intToWord 50 = "fifty"
intToWord 60 = "sixty"
intToWord 70 = "seventy"
intToWord 80 = "eighty"
intToWord 90 = "ninety"
intToWord 1000 = "onethousand"
intToWord n | n >= 20 && n < 100 = 
                let (tens, ones) = n `divMod` 10
                in
                  intToWord (tens*10) ++ intToWord ones
            | n >= 100 && n < 1000 = 
                let (hundreds, tens) = n `divMod` 100
                in
                  intToWord hundreds ++ "hundred" ++ 
                            if tens == 0 
                             then ""
                             else "and" ++ intToWord tens
            | otherwise = error "Not defined outside [1-1000]"

numLetters :: Int -> Int
numLetters = length . intToWord

counts :: Int -> Int -> [Int]
counts start end = map numLetters [start..end]

theAnswer :: Int
theAnswer = sum $ counts 1 1000