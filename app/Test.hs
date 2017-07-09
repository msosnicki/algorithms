module Test where

import Data.Char
import Control.Applicative
import System.IO

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNums 2 >>= \nums ->
  putStrLn $ show $ sum nums

nextNums :: (Integral a, Read a) => Int -> IO [a]
nextNums n = sequence $ take n (repeat nextNum)

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' :: (Integral a, Read a) => String -> IO a
nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
