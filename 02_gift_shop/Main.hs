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
parseRanges =
  map parseRange . filter (not . null) . splitOn ',' . filter (not . isSpace)
  where
    parseRange tok =
      case span (/= '-') tok of
        (lo, '-':hi) -> (read lo, read hi)
        _            -> error ("Malformed range: " ++ tok)

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
invalidPart1 n =
  let digits     = show n
      len        = length digits
      halfLength = len `div` 2
      (prefix, suffix) = splitAt halfLength digits
  in even len && prefix == suffix

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
