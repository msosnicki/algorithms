{-# LANGUAGE TupleSections #-}
module Problem1 where

import Data.Char
import Data.List
import Data.Array
import Data.Maybe
import Data.Ord
import Control.Applicative
import System.IO

-- Primitive calculator

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  let (count, solution) = solve n
      printSize = putStrLn $ show count
      printList = putStrLn $ intercalate " " $ show <$> solution
  in printSize >> printList

solve :: Int -> (Int, [Int])
solve n = (count lastCell, reconstruct solution n)
  where
    lastCell = solution ! n
    solution = array (1, n) $ (1, Cell 0 1 0) : [
      (i, best) | i <- [2..n],
        let prevCells = catMaybes $ prevCellsF i solution <$> ops
            (bestCell, bestIndex, op) = head $ sortBy (comparing (count . fst3)) prevCells
            best = Cell bestIndex (function op $ value bestCell) (count bestCell + 1)
      ]

reconstruct arr n = loop n []
  where loop i acc =
          let Cell prev value _ = arr ! i
          in if (prev==0) then value : acc else loop prev (value : acc)

fst3 (e, _, _) = e

prevCellsF i arr op = (\i -> (arr ! i, i, op)) <$> index
  where index = inverse op i

prevCellsF2 i arr op = (, op) <$> prev
  where inv = inverse op i
        prev = (arr !) <$> inv
        
data Op = Op {function :: Int -> Int, inverse :: Int -> Maybe Int}

data Cell = Cell {prev :: Int, value :: Int, count :: Int} deriving Show

ops = [
  Op (+1) (Just . (subtract 1)),
  Op (*3) (invMul 3),
  Op (*2) (invMul 2)
      ]

invMul times = f
  where f num =
          let (d, r) = num `divMod` times
          in if(r == 0) then Just d else Nothing


nextNums :: (Integral a, Read a) => Int -> IO [a]
nextNums n = sequence $ replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
