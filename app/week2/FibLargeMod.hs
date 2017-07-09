module FibLargeMod where

import Data.Char
import Control.Applicative
import System.IO

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNum >>= \m ->
  putStrLn $ show $ fibsModN n m

fibsModN :: Integer -> Integer -> Integer
fibsModN n m =
  let p = (subseqAfter (drop 2 $ fibsMod m) ([0,1])) + 2
      r = n `rem` p
  in fibsMod m !! fromInteger r

subseqAfter :: (Eq a) => [a] -> [a] -> Integer
subseqAfter l i
  | startsWith l i = 0
  | otherwise = 1 + subseqAfter (tail l) i

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (h1 : t1) (h2 : t2)
  | h1 == h2 = startsWith t1 t2
  | otherwise = False

fibsMod m = map (`mod` m) fibs

fibs :: [Integer]
fibs  = scanl (+) 0 (1:fibs)

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' :: (Integral a, Read a) => String -> IO a
nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
