module Lcm where

import Data.Char
import Control.Applicative
import System.IO

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \a ->
  nextNum >>= \b ->
  putStrLn $ show $ lcm' a b

lcm' :: Integer -> Integer -> Integer
lcm' a b =
  let g = gcd a b
      b' = b `div` g
  in a * b'
  
nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' :: (Integral a, Read a) => String -> IO a
nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
