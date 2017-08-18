module Problem3 where

import Data.Char
import Data.List
import Data.Array
import Control.Applicative
import System.IO

-- Distance between strings

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  readString >>= \str1 ->
  readString >>= \str2 ->
  putStrLn $ show $ distance str1 str2

distance as bs = solution ! (aSize, bSize)
  where aSize = length as
        bSize = length bs
        solution = array ((0, 0), (aSize, bSize)) $
          [((a, 0), a) | a <- [0..aSize]] ++
          [((0, b), b) | b <- [1..bSize]] ++
          [((a, b), best) | a <- [1..aSize], b <- [1..bSize],
            let best = minimum [solution ! (a-1, b) + 1, solution ! (a, b - 1) + 1,
                                let x = solution ! (a - 1, b - 1)
                                    aa = as !! (a - 1)
                                    bb = bs !! (b - 1)
                                in if(aa == bb) then x else x + 1
                               ]
            ]

readString :: IO String
readString = getLine
