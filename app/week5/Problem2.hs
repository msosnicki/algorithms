module Problem2 where

import Data.Char
import Data.List
import Data.Array
import Control.Applicative
import System.IO

-- Take as much gold as you can 

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \cap ->
  nextNum >>= \it ->
  nextNums it >>= \items ->
  putStrLn $ show $ solve cap $ sort items

solve :: Int -> [Int] -> Int
solve cap items = solution ! (cap, itemsCount)
  where
    itemsCount = length items
    itemsArray = array (1, itemsCount) $ zip [1..itemsCount] items
    solution = array ((0,0), (cap, itemsCount)) $
          [((0, i),0) | i <- [0..itemsCount]] ++
          [((c, 0),0) | c <- [1..cap]] ++ [
          ((c, i), best) | i <- [1..itemsCount], c <- [1..cap],
            let bestWithout = solution ! (c, i - 1)
                ind = c - (itemsArray ! i)
                bestWith = if(ind < 0) then 0 else (solution ! (ind, i - 1)) + (itemsArray ! i)
                best = max bestWithout (if(bestWith > c) then 0 else bestWith)]

nextNums :: (Integral a, Read a) => Int -> IO [a]
nextNums n = sequence $ replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
