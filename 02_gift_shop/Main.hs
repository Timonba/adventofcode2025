{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char (isSpace)
import Data.List (isInfixOf)

type Range = (Integer, Integer)

main :: IO ()
main = do
  input <- getContents

  -- Parse "a-b,c-d,..." into [(a,b), (c,d), ...]
  let ranges = parseRanges input

      part1 = sum
        [ n
        | (lo, hi) <- ranges
        , n <- [lo .. hi]
        , invalidPart1 n
        ]

      part2 = sum
        [ n
        | (lo, hi) <- ranges
        , n <- [lo .. hi]
        , invalidPart2 n
        ]

  print part1
  print part2

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

parseRanges :: String -> [Range]
parseRanges s =
  [ let (a, bWithDash) = break (== '-') tok
        b              = drop 1 bWithDash
    in (read a, read b)
  | tok <- splitOn ',' cleaned
  , not (null tok)
  ]
  where
    -- Remove all whitespace (newlines, spaces, etc.) just in case
    cleaned = filter (not . isSpace) s

-- Simple split on a single character
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c xs =
  let (chunk, rest) = break (== c) xs
  in chunk : case rest of
               []      -> []
               (_:xs') -> splitOn c xs'

------------------------------------------------------------
-- Part 1: "some sequence of digits repeated twice"
------------------------------------------------------------

invalidPart1 :: Integer -> Bool
invalidPart1 = maybe False (uncurry (==)) . splitEvenHalves . show

-- Uses tortoise/hare pointers: the fast cursor moves twice as
-- quickly as the slow one, so when fast reaches the end the slow
-- cursor sits at the midpoint and we have both halves without ever
-- computing the length.
splitEvenHalves :: [a] -> Maybe ([a], [a])
splitEvenHalves xs = go xs xs []
  where
    go (s:slow') (_:_:fast') acc = go slow' fast' (s:acc)
    go [] (_:_:_) _             = Nothing
    go _ [_] _                  = Nothing
    go slow [] acc              = Just (reverse acc, slow)

------------------------------------------------------------
-- Part 2: "some sequence of digits repeated at least twice"
------------------------------------------------------------

invalidPart2 :: Integer -> Bool
invalidPart2 n =
  let s  = show n
      ss = s ++ s
      -- classic trick: s is made of a repeated substring
      -- iff it appears inside (s ++ s) with first and last
      -- character removed
      inner = tail (init ss)
  in length s > 1 && s `isInfixOf` inner
