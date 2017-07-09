module Gcd where

import Data.Char
import Control.Applicative
import System.IO

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \a ->
  nextNum >>= \b ->
  putStrLn $ show $ gcd' a b

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b =
  let a' = a `mod` b
  in gcd' b a'

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' :: (Integral a, Read a) => String -> IO a
nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
