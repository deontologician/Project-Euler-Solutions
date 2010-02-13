module Q22 where

{-
Question 22

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
containing over five-thousand first names, begin by sorting it into alphabetical
order. Then working out the alphabetical value for each name, multiply this
value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

-}

import qualified Data.ByteString.Char8 as L
import Data.List (sort, foldl')

loadFile :: String -> IO [String]
loadFile filename = do
  file <- L.readFile filename
  return $ sort $ fmap (read . L.unpack) $ L.split ',' file

alphaScore :: String -> Int
alphaScore = foldl' (\y x -> y + (fromEnum x) - 64) 0

totalScore :: [String] -> Int
totalScore = snd . foldl' (\(i,t) x -> (i+1, i*(alphaScore x) + t)) (1,0)

theAnswer :: IO Int
theAnswer = do
  names <- loadFile "names.txt"
  return $ totalScore names