module Problem1 where

import Data.Char
import Data.List
import Data.Array
import Data.Maybe
import Control.Applicative
import System.IO

-- Primitive calculator

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  let result = solve n
      printSize = putStrLn $ show $ length result - 1
      printList = putStrLn $ intercalate " " (show <$> result)
  in printSize >>= \_ -> printList
  

solve :: Integer -> [Integer]
solve n = reverse $ solution ! n
  where
    solution = array (1, n) $ (1, [1]) : [
      (i, (op $ fromJust index) : (fromJust acc)) | i <- [2..n],
        let solutions = sol i solution <$> ops
            sorted = sortBy sorting solutions
            (op, index, acc) = head sorted
      ]

sorting (_, _, Nothing) (_, _, l2) = GT
sorting (_, _, l1) (_, _, Nothing) = LT
sorting (_, _, Just l1) (_, _, Just l2) = compare (length l1) (length l2)

data Op = Op {function :: Integer -> Integer, inverse :: Integer -> Maybe Integer}

ops = [
  Op (+1) (Just . (subtract 1)),
  Op (*3) (invMul 3),
  Op (*2) (invMul 2)
      ]

sol num arr op =
  let i = inverse op num
  in (function op, i, (arr !) <$> i)

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
