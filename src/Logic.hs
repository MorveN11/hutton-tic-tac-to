module Logic
  ( wins,
    move,
    full,
    next,
    turn,
    won,
    valid,
  )
where

import Constants (size)
import Data.List (transpose)
import Game (Grid, Player (B, O, X))

next :: Player -> Player
next O = X
next B = B
next X = O

full :: Grid -> Bool
full = all (notElem B)

turn :: Grid -> Player
turn grid = if os <= xs then O else X
  where
    ps = concat grid
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)

diag :: Grid -> [Player]
diag grid = [grid !! n !! n | n <- [0 .. size - 1]]

wins :: Player -> Grid -> Bool
wins player grid = any line (rows ++ cols ++ dias)
  where
    line = all (== player)
    rows = grid
    cols = transpose grid
    dias = [diag grid, diag (map reverse grid)]

won :: Grid -> Bool
won grid = wins O grid || wins X grid

valid :: Grid -> Int -> Bool
valid grid i = 0 <= i && i < (size ^ (2 :: Int)) && concat grid !! i == B

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move grid index player =
  [chop size (xs ++ [player] ++ tail ys) | valid grid index]
  where
    (xs, ys) = splitAt index (concat grid)
