module Render (putGrid, playerAsString, playerThinkingAsString) where

import Constants (size)
import Game (Grid, Player (B, O, X))

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

putGrid :: Grid -> String
putGrid =
  unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

playerAsString :: Player -> String
playerAsString player = "Player " ++ show player ++ ", enter your move: "

playerThinkingAsString :: Player -> String
playerThinkingAsString player = "Player " ++ show player ++ ", is thinking..."
