module Problem2 where

import Data.Char
import Data.List
import Control.Applicative
import System.IO
import Text.Printf

-- The goal of this code problem is to implement an algorithm for the fractional knapsack problem.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNum >>= \w ->
  nextItems n >>= \items ->
  putStrLn $ print4 $ solve w items


print4 :: (Fractional a, PrintfArg a) => a -> String
print4 x = printf "%.4f" x

solve :: Integer -> [Item] -> Double
solve 0 _ = 0
solve _ [] = 0
solve c (i@(Item v w) : tail)
  | w < c = fromIntegral v + solve (c - w) tail
  | w > c = (valuePerUnit i) * (fromIntegral c)
  | w == c = fromIntegral v

nextItems n = fmap (sortBy (flip compare)) $ sequence $ take n $ repeat nextItem

nextItem :: IO Item
nextItem =
  nextNum >>= \v ->
  fmap (Item v) nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n


data Item = Item {value :: Integer, weight :: Integer} deriving (Show, Eq)

instance Ord Item where
  compare i1 i2 = compare (valuePerUnit i1) (valuePerUnit i2)

valuePerUnit (Item a b)= fromIntegral a / fromIntegral b
