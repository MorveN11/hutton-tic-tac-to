module Constants
  ( depth,
    size,
    cls,
    goto,
    playerOWins,
    playerXWins,
    draw,
    choosePlayerString,
    errorInvalidChoice,
    errorInvalidNumber,
    errorInvalidMove,
    errorHandleTurn,
  )
where

type Pos = (Int, Int)

depth :: Int
depth = 9

size :: Int
size = 3

cls :: String
cls = "\ESC[2J"

goto :: Pos -> String
goto (x, y) = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

playerOWins :: String
playerOWins = "Player O wins!\n"

playerXWins :: String
playerXWins = "Player X wins!\n"

draw :: String
draw = "It's a draw\n"

choosePlayerString :: String
choosePlayerString = "Do you want to play first or second? Enter 1 for first, 2 for second: "

errorInvalidChoice :: String
errorInvalidChoice = "Invalid choice. Please enter 1 or 2."

errorInvalidNumber :: String
errorInvalidNumber = "ERROR: Invalid Number"

errorInvalidMove :: String
errorInvalidMove = "ERROR: Invalid Move"

errorHandleTurn :: String
errorHandleTurn = "ERROR: Cant handle this turn"
