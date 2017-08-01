{-# LANGUAGE ViewPatterns #-}
module Problem6 where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.State
import System.IO

type Point = (Integer, Integer)

-- The goal in this problem is to find the minimum number of coins needed to change the input value
-- (an integer) into coins with denominations 1, 5, and 10.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \p ->
  nextPoints p >>= \pointsAndRange ->
  let (points, (xmin, xmax)) = pointsAndRange
  in putStrLn $ show $ solve xmin xmax points

solve _ _ [] = Nothing
solve _ _ (h:[]) = Nothing
solve _ _ (a:b:[]) = Just $ distance a b
solve xmin xmax points =
  let sortX (x1, _) (x2, _) = compare x1 x2
      sorted = sortBy sortX points
      mid = (xmin+) $ (xmax - xmin) `div` 2
      (left, right) = partition ((<mid) . fst) sorted
      resl = solve xmin mid left
      resr = solve mid xmax right
      cmin = fromMaybe 10000000000000 $ (liftA2 max resl resr) <|> resl <|> resr
      middle = solve (xmin - (floor cmin)) (xmin + (ceiling cmin)) (dropWhile (distanceX mid cmin) left ++ takeWhile (distanceX mid cmin) right)
  in middle >>= (\m -> if(m < cmin) then Just m else Just cmin)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ ((x1 - x2)^2) + ((y1 - y2)^2)

distanceX :: Integer -> Float -> Point -> Bool
distanceX x d (xp, _)=
  let dist = abs $ x - xp
  in (<d) $ fromIntegral $ dist 
  
nextPoints n = runStateT (next nextPointS n) (0, 0)

next a n = sequence $ replicate n a

nextPointS :: StateT Point IO Point
nextPointS = do
  s <- get
  point <- io $ nextPoint
  let xmin = min (fst s) (fst point)
  let xmax = max (snd s) (fst point)
  put (xmin, xmax)
  return point

io :: IO a -> StateT Point IO a
io = liftIO

nextPoint =
  nextNum >>= \n1 ->
  fmap (\n2 -> (n1, n2)) nextNum

nextNums = next nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char || (null n && char == '-')) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n

data ElementType = Up | Point | Down deriving (Show, Eq, Ord)
