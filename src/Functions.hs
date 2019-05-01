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

-- Get Diagonals: (1) -> 1 5 9, (2) -> 7 5 3
getDiagonal :: Board -> Int -> Match
getDiagonal board diagonal
    | diagonal == 1 = [ ((getColumn board 0) !! 0), ((getColumn board 1) !! 1), ((getColumn board 2) !! 2) ]
    | diagonal == 2 = [ ((getColumn board 2) !! 2), ((getColumn board 1) !! 1), ((getColumn board 0) !! 0) ]

-- Checking successfull match
itsAMatch :: Match -> Value
itsAMatch row 
    | sum(row) == 3 = x 
    | sum(row) == 0 = o
    | otherwise = -1
-- | empty `elem` row = empty

-- Checking winner
hasWinner :: Board -> Player
hasWinner board
    | itsAMatch (getRow board 0) == x || itsAMatch (getRow board 1) == x || itsAMatch (getRow board 2) == x = x
    | itsAMatch (getRow board 0) == o || itsAMatch (getRow board 1) == o || itsAMatch (getRow board 2) == o = o
    | itsAMatch (getColumn board 0) == x || itsAMatch (getColumn board 1) == x || itsAMatch (getColumn board 2) == x = x
    | itsAMatch (getColumn board 0) == o || itsAMatch (getColumn board 1) == o || itsAMatch (getColumn board 2) == o = o
    | itsAMatch (getDiagonal board 0) == x || itsAMatch (getDiagonal board 1) == x = x
    | itsAMatch (getDiagonal board 0) == o || itsAMatch (getDiagonal board 1) == o = o
    | otherwise = -1
