module TicTacToe (play) where

import AI (bestMove)
import Constants
  ( choosePlayerString,
    cls,
    draw,
    errorInvalidChoice,
    errorInvalidMove,
    errorInvalidNumber,
    goto,
    playerOWins,
    playerXWins,
  )
import Data.Char (isDigit)
import Game (Grid, Player (O, X), initialGame)
import Logic (full, move, next, wins)
import Render (playerAsString, playerThinkingAsString, putGrid)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )

choosePlayer :: IO Player
choosePlayer = do
  putStrLn choosePlayerString
  choice <- getLine
  case choice of
    "1" -> return O
    "2" -> return X
    _ -> do
      putStrLn errorInvalidChoice
      choosePlayer

getNat :: String -> IO Int
getNat prompt =
  do
    putStrLn prompt
    xs <- getLine
    if xs /= [] && all isDigit xs
      then return (read xs)
      else do
        putStrLn errorInvalidNumber
        getNat prompt

run' :: Grid -> Player -> Player -> IO ()
run' grid player initialPlayer
  | wins O grid = putStrLn playerOWins
  | wins X grid = putStrLn playerXWins
  | full grid = putStrLn draw
  | player == initialPlayer =
      do
        index <- getNat $ playerAsString player
        case move grid index player of
          [] -> do
            putStrLn errorInvalidMove
            run' grid player initialPlayer
          (grid' : _) -> run grid' (next player) initialPlayer
  | otherwise =
      do
        putStrLn $ playerThinkingAsString player
        (run $! bestMove grid player) (next player) initialPlayer

run :: Grid -> Player -> Player -> IO ()
run grid player initialPlayer =
  do
    putStrLn cls
    putStr $ goto (1, 1)
    putStrLn $ putGrid grid
    run' grid player initialPlayer

play :: IO ()
play =
  do
    hSetBuffering stdout NoBuffering
    putStrLn cls
    putStr $ goto (1, 1)
    initialPlayer <- choosePlayer
    run initialGame O initialPlayer
