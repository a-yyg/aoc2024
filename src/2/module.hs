module Main where
import Data.List
import Data.Maybe
import Data.Char
import System.Environment
import Debug.Trace (trace)

newtype Report = Report [Int]
newtype Problem = Problem [Report]

instance Show Report where
  show (Report rs) = show rs


readInput :: String -> Problem
readInput s = Problem $ map readReport $ lines s

readReport :: String -> Report
readReport s = Report [read x :: Int | x <- words s]

isMonotonic :: (Int -> Int -> Bool) -> [Int] -> Bool
isMonotonic ord [a,b] = ord a b
isMonotonic ord (a:(b:cs@(c:ds))) =
  (ord a b && ord b c) && isMonotonic ord cs
isMonotonic ord [_] = True

isRange [a,b] =
  delta > 0 && delta < 4
  where delta = abs (a - b)
isRange (a:bs@(b:cs)) =
  isRange [a,b] && isRange bs
isRange [_] = True

isSafe :: Report -> Bool
isSafe r1@(Report r) =
  (isMonotonic (>) r || isMonotonic (<) r) && isRange r

data Part = PartOne | PartTwo

solve :: Part -> (Problem -> Int)
solve p = case p of
  PartOne -> solve1
  PartTwo -> solve2

readPart :: IO Part
readPart = do
  args <- getArgs
  case args of
    ("1":_) -> return $ PartOne
    ("2":_) -> return $ PartTwo
    (c:_) -> error $ "invalid part: " ++ show c
    [] -> error $ "problem part not provided. provide part=1 or 2"


solve1 :: Problem -> Int
solve1 (Problem rs) = length $ filter id result
  where result = map isSafe rs


subLists :: [a] -> [[a]]
subLists [] = []
subLists (x:xs) = xs : [x:xs' | xs' <- subLists xs]

subReports :: Report -> [Report]
subReports (Report rs) = [Report r | r <- subLists rs]

isSemiSafe :: Report -> Bool
isSemiSafe r = any isSafe (subReports r)

solve2 :: Problem -> Int
solve2 (Problem rs) = length $ filter id result
  where result = map isSemiSafe rs

main :: IO()
main = do
  part <- readPart
  inp <- getContents
  print $ ((solve part) . readInput) inp
