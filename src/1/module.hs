module Main where
import Data.List
import Data.Maybe
import Data.Char
import System.Environment
-- import qualified Data.MultiSet as Mset

readInput :: String -> [(Int, Int)]
readInput s = mapMaybe parseLine $ lines s

parseLine :: String -> Maybe (Int, Int)
parseLine s =
  case words s of
    [a, b] -> Just (read a :: Int, read b :: Int)
    [] -> Nothing


solve1 :: [(Int, Int)] -> Int
solve1 w = solve1_ a b
  where (a0, b0) = unzip w
        a = sort a0
        b = sort b0

solve1_ :: [Int] -> [Int] -> Int
solve1_ [] [] = 0
solve1_ (a:as) (b:bs) = abs (a - b) + solve1_ as bs


solve2 :: [(Int, Int)] -> Int
solve2 w = solve2_ a b
  where (a, b) = unzip w

-- solve2_ :: [Int] -> [Int] -> Int
-- solve2_ a b = foldr f as
--   where am = Mset.fromList a
--         bm = Mset.fromList b
--         as = Mset.toSet am
--         fa = Mset.toCountMap am
--         fb = Mset.toCountMap bm
--         f x = (fa x) * (fb x)

type ID = Int
type MultiSet = [(ID, Int)]

listToMS :: [Int] -> MultiSet
listToMS l = foldr appendMS [] l

appendMS :: ID -> MultiSet -> MultiSet
appendMS i [] = [(i,1)]
appendMS i (m@(k,v):ms) =
  if (i == k) then
    ((k,v+1):ms)
  else m:appendMS i ms

findMS :: MultiSet -> ID -> Int
findMS [] i = 0
findMS ((k,v):ms) i =
  if (i == k) then v
  else findMS ms i

solve2_ :: [Int] -> [Int] -> Int
solve2_ a b = foldr (\x acc -> x * (fb x) + acc) 0 as
  where am = listToMS a
        bm = listToMS b
        (as, ac) = unzip am
        fb = findMS bm

data Part = PartOne | PartTwo

solve :: Part -> ([(Int, Int)] -> Int)
solve p = case p of
  PartOne -> solve1
  PartTwo -> solve2

readPart :: IO(Part)
readPart = do
  args <- getArgs
  case args of
    ("1":_) -> return $ PartOne
    ("2":_) -> return $ PartTwo
    (c:_) -> error $ "invalid part: " ++ (show c)
    [] -> error $ "problem part not provided. provide part=1 or 2"


main :: IO()
main = do
  part <- readPart
  inp <- getContents
  print $ ((solve part) . readInput) inp
