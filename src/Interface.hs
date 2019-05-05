-- Interface
module Interface where 

-- Imports
import Types 
import Functions
import Constants

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

getPlayer :: Int -> Player
getPlayer round 
    | round `mod` 2 == 0 = x
    | otherwise = o

start :: Board -> Int -> IO()
start board round = do    
    -- clear
    drawBoard board
    let player = getPlayer round
    putStrLn $ "Vez de : " ++ (getCharPlayer player)

    putStr "Infome a posição: "
    position <- getLine

    let valid = (getValidPosition position)
    if(valid /= -1) then do
        let newB = play board player valid

        let winner = hasWinner newB

        if(winner == -1) then do
            start newB (round+1)
        else do
            clear
            putStrLn $ "O vencedor é: " ++ (getCharPlayer winner)
            drawBoard newB
            return();
    else do
        start board round

getCharPlayer :: Value -> String
getCharPlayer player
    | player == 1 = " X "
    | player == 0 = " O "
    | otherwise = " _ "

drawPosition :: Board -> Position -> Position -> String
drawPosition board row column = getCharPlayer ((getRow board row) !! column)

drawRow :: Board -> Int -> String
drawRow board row = " " ++ (drawPosition board row 0) ++ " | " ++ (drawPosition board row 1) ++ " | " ++ (drawPosition board row 2) ++ " "


drawBoard :: Board -> IO ()
drawBoard board = do 
    putStrLn $ (drawRow board 0) 
    putStrLn "__________________"
    putStrLn $ (drawRow board 1)
    putStrLn "__________________"
    putStrLn $ (drawRow board 2)

-- Auxiliar Board

drawAuxPosition :: Board -> Position -> Position -> Int
drawAuxPosition board row column = ((getRow board row) !! column)

drawAuxRow :: Board -> Int -> String
drawAuxRow board row = " " ++ show(drawAuxPosition board row 0) ++ " | " ++ show(drawAuxPosition board row 1) ++ " | " ++ show(drawAuxPosition board row 2) ++ " "


drawAuxBoard :: Board -> IO ()
drawAuxBoard board = do 
    putStrLn $ (drawAuxRow board 0) 
    putStrLn "___________"
    putStrLn $ (drawAuxRow board 1)
    putStrLn "___________"
    putStrLn $ (drawAuxRow board 2)