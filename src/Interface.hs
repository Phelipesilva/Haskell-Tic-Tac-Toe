-- Interface
module Interface where 

-- Imports
import Types 

getValidPosition :: String -> Position
getValidPosition input
    | input == "1" = 1
    | input == "2" = 2
    | input == "3" = 3
    | input == "4" = 4
    | input == "5" = 5
    | input == "6" = 6
    | input == "7" = 7
    | input == "8" = 8
    | input == "9" = 9
    | otherwise = -1

playing :: IO -> Board -> Value
playing position board
    | position > -1 = playing getLine board 
    | otherwise = playing position board
