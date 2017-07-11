module Lastdigitfib where

import Data.Char
import Control.Applicative
import System.IO

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \num ->
  putStrLn $ show $ lastDigitOfSum num

lastDigitOfSum n =
  let pp = pisanoPeriod 10
      s = n `div` pp
      r = n `rem` pp
      sum1 = (s*) $ sum $ take pp fibsMod10
      sum2 = sum $ take r fibsMod10
  in sum1 + sum2 `mod` 10

pisanoPeriod m =
  let pp l acc = case l of
        (0:1:_)    -> acc + 2
        (_:0:t)    -> let acc' = 1 + acc in seq acc' $ pp (0:t) acc'
        (h1:h2:t)  -> let acc' = 2 + acc in seq acc' $ pp t acc'
  in pp (drop 2 $ fibsMod m) 0

fibsMod10 = fibsMod 10
fibsMod m = map (`mod` m) fibs

fibs  = scanl (+) 0 (1:fibs)

nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
