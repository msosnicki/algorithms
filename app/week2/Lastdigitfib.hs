module Lastdigitfib where

import Data.Char
import Control.Applicative
import System.IO

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \num ->
  putStrLn $ show $ fibs !! num

fibs :: [Integer]
fibs  = map (`mod` 10) $ scanl (+) 0 (1:fibs)

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' :: (Integral a, Read a) => String -> IO a
nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
