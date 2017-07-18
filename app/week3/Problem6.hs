module Problem6 where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import System.IO

-- Given two sequences a1, a2.. an (ai is the profit per click of the i-th ad) and b1, b2, . . . , bn (bi is
-- the average number of clicks per day of the i-th slot), we need to partition them into n pairs (ai, bi))
-- such that the sum of their products is maximized.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \nums ->
  putStrLn $ show $ solve nums 

solve nums = toNum $ reverse $ sortBy order nums

toNum :: (Integral a, Show a) => [a] -> a
toNum = foldl' f 0
  where f acc elem =
          let n = (10^) $ (1+) $ pow elem
          in acc * n + elem

order :: Integer -> Integer -> Ordering
order a b
  | a == b = EQ
  | ah == bh = (fromMaybe ah at) `order` (fromMaybe bh bt)
  | otherwise = compare ah bh
  where (ah, at) = mostImportant a
        (bh, bt) = mostImportant b

mostImportant :: Integer -> (Integer, Maybe Integer)
mostImportant num =
  let n = pow num
      (m, r) = divMod num $ 10^n
  in (m, if(r == 0) then Nothing else Just r)

pow num = (-1+) $ length $ show num
  
nextNums :: (Integral a, Read a) => Int -> IO [a]
nextNums n = sequence $ take n $ repeat nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char || (null n && char == '-')) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
