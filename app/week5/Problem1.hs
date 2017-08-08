module Problem1 where

import Data.Char
import Data.List
import Data.Array
import Control.Applicative
import System.IO

-- Primitive calculator

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  putStrLn $ show $ solve n

solve :: Integer -> Integer
solve n = solution ! n
  where
    solution = array (1, n) $ (1, 0) : [
      (i, best + 1) | i <- [2..n],
        let best = minimum $ (solution !) <$> (ind i)
      ]

ind n =
  let is = [(n `divMod` 3), (n `divMod` 2), (n - 1, 0)]
      f = filter ((==0) . snd) $ filter ((>0) .fst) is
  in fst <$> f

nextNums :: (Integral a, Read a) => Int -> IO [a]
nextNums n = sequence $ replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
