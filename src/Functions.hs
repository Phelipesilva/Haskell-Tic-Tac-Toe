-- Functions
module Functions where

-- Imports
import Types 
import Constants 

-- Get Row
getRow :: Board -> Int -> Match
getRow board row = board !! row

-- Get Column
getColumn :: Board -> Int -> Match
getColumn board column = [ ((board !! 0) !! column), ((board !! 1) !! column), ((board !! 2) !! column) ]

-- Get Column of position 
getRealColumn :: Position -> Position
getRealColumn position
    | position == 1 || position == 4 || position == 7 = 0
    | position == 2 || position == 5 || position == 8 = 1
    | position == 3 || position == 6 || position == 9 = 2

-- Get Diagonals: (1) -> 1 5 9, (2) -> 7 5 3
getDiagonal :: Board -> Int -> Match
getDiagonal board diagonal
    | diagonal == 1 = [ ((getColumn board 0) !! 0), ((getColumn board 1) !! 1), ((getColumn board 2) !! 2) ]
    | diagonal == 2 = [ ((getColumn board 2) !! 2), ((getColumn board 1) !! 1), ((getColumn board 0) !! 0) ]

-- Checking successfull match
itsAMatch :: Match -> Value
itsAMatch match 
    | sum(match) == 3 = x 
    | sum(match) == 0 = o
    | otherwise = -1

-- Checking winner
hasWinner :: Board -> Value
hasWinner board
    | itsAMatch (getRow board 0) == x || itsAMatch (getRow board 1) == x || itsAMatch (getRow board 2) == x = x
    | itsAMatch (getRow board 0) == o || itsAMatch (getRow board 1) == o || itsAMatch (getRow board 2) == o = o
    | itsAMatch (getColumn board 0) == x || itsAMatch (getColumn board 1) == x || itsAMatch (getColumn board 2) == x = x
    | itsAMatch (getColumn board 0) == o || itsAMatch (getColumn board 1) == o || itsAMatch (getColumn board 2) == o = o
    | itsAMatch (getDiagonal board 1) == x || itsAMatch (getDiagonal board 1) == x = x
    | itsAMatch (getDiagonal board 2) == o || itsAMatch (getDiagonal board 1) == o = o
    | otherwise = -1

-- Make a move
makeAMove :: Match -> Position -> Player -> Match
makeAMove row position player = take position (row) ++ [player] ++ drop (position + 1) (row)

-- Play
play :: Board -> Player -> Position -> Board
play board player position
    | position <= 3 = [ (makeAMove (getRow board 0) (getRealColumn position) player), (getRow board 1), (getRow board 2) ]
    | position <= 6 = [ (getRow board 0), (makeAMove (getRow board 1) (getRealColumn position) player), (getRow board 2) ]
    | position <= 9 = [ (getRow board 0), (getRow board 1), makeAMove (getRow board 2) (getRealColumn position) player ]