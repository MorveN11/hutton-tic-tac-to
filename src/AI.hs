module AI (bestMove) where

import Constants (depth, errorHandleTurn, size)
import Game (Grid, Player (B, O, X))
import Logic (full, move, next, turn, wins, won)

data Tree a = Node a [Tree a]
  deriving (Show)

moves :: Grid -> Player -> [Grid]
moves grid player
  | won grid = []
  | full grid = []
  | otherwise = concat [move grid index player | index <- [0 .. ((size ^ (2 :: Int)) - 1)]]

gameTree :: Grid -> Player -> Tree Grid
gameTree grid player = Node grid [gameTree grid' (next player) | grid' <- moves grid player]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node grid [])
  | wins O grid = Node (grid, O) []
  | wins X grid = Node (grid, X) []
  | otherwise = Node (grid, B) []
minimax (Node grid ts)
  | turn grid == O = Node (grid, minimum ps) ts'
  | turn grid == X = Node (grid, maximum ps) ts'
  | otherwise = error errorHandleTurn
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

bestMove :: Grid -> Player -> Grid
bestMove grid player = head [grid' | Node (grid', player') _ <- ts, player' == best]
  where
    tree = prune depth (gameTree grid player)
    Node (_, best) ts = minimax tree
