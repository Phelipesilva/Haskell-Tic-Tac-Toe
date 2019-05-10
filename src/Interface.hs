-- Interface
module Interface where 

-- Imports
import Types 
import Functions
import Constants

verifyPositions :: Int -> [Int] -> Bool
verifyPositions n xs
  | n == head xs = False
  | length xs > 1 = verifyPositions n (tail xs)
  | otherwise = True

getValidPosition :: String -> [Int] -> Position
getValidPosition input positions
    | input == "1" && verifyPositions 1 positions = 1
    | input == "2" && verifyPositions 2 positions = 2
    | input == "3" && verifyPositions 3 positions = 3
    | input == "4" && verifyPositions 4 positions = 4
    | input == "5" && verifyPositions 5 positions = 5
    | input == "6" && verifyPositions 6 positions = 6
    | input == "7" && verifyPositions 7 positions = 7
    | input == "8" && verifyPositions 8 positions = 8
    | input == "9" && verifyPositions 9 positions = 9
    | otherwise = -1

verifyPositionWinner :: [Int] -> String
verifyPositionWinner positions
    | verifyPositions 1 positions && verifyPositions 4 positions = "6"
    | verifyPositions 1 positions && verifyPositions 6 positions = "4"
    | verifyPositions 4 positions && verifyPositions 6 positions = "1"
    | verifyPositions 2 positions && verifyPositions 5 positions = "8"
    | verifyPositions 2 positions && verifyPositions 8 positions = "5"
    | verifyPositions 5 positions && verifyPositions 8 positions = "2"
    | verifyPositions 3 positions && verifyPositions 6 positions = "9"
    | verifyPositions 3 positions && verifyPositions 9 positions = "6"
    | verifyPositions 6 positions && verifyPositions 9 positions = "3"
    | verifyPositions 1 positions && verifyPositions 2 positions = "3"
    | verifyPositions 1 positions && verifyPositions 3 positions = "2"
    | verifyPositions 2 positions && verifyPositions 3 positions = "1"
    | verifyPositions 4 positions && verifyPositions 5 positions = "6"
    | verifyPositions 4 positions && verifyPositions 6 positions = "5"
    | verifyPositions 5 positions && verifyPositions 6 positions = "4"
    | verifyPositions 7 positions && verifyPositions 8 positions = "9"
    | verifyPositions 7 positions && verifyPositions 9 positions = "8"
    | verifyPositions 8 positions && verifyPositions 9 positions = "7"
    | verifyPositions 1 positions && verifyPositions 5 positions = "9"
    | verifyPositions 1 positions && verifyPositions 9 positions = "5"
    | verifyPositions 5 positions && verifyPositions 9 positions = "1"
    | verifyPositions 3 positions && verifyPositions 5 positions = "7"
    | verifyPositions 3 positions && verifyPositions 7 positions = "5"
    | verifyPositions 5 positions && verifyPositions 7 positions = "3"
    | otherwise = "-1"

verifyCorner :: [Int] -> String
verifyCorner positions
    | verifyPositions 1 positions = "1"
    | verifyPositions 3 positions = "3"
    | verifyPositions 7 positions = "7"
    | verifyPositions 9 positions = "9"
    | otherwise = "-1"

gerateCPUPosition :: [Int]-> [Int] -> String
gerateCPUPosition positionsX positionsO
    | verifyPositionWinner positionsX /= "-1" = verifyPositionWinner positionsX
    | verifyPositionWinner positionsO /= "-1" = verifyPositionWinner positionsO
    | verifyCorner positionsX++positionsO /="-1" = verifyCorner (positionsX++positionsO)
    | otherwise = "-1"


getPlayer :: Int -> Player
getPlayer round 
    | round `mod` 2 == 0 = x
    | otherwise = o

geratePosition ::[Int] -> [Int] -> Int -> Player -> String
geratePosition positionsX positionsO round player =
    if (mod round 2 /= 0) then do
        putStrLn "\n------------------------------"
        drawAuxBoard auxBoard
        putStrLn $ "\nVez de : " ++ (getCharPlayer player)
        putStr "> Infome a posição: "
        position <- getLine
        putStr "\n"
        position
    else do
        gerateCPUPosition positionsX positionsO

start :: Board -> Int-> [Int] -> [Int] -> IO()
start board round positionsX positionsO = do
    drawBoard board
    let player = getPlayer round

    let position = geratePosition positionsX positionsO round player
    let valid = (getValidPosition position (positionsX++positionsO))
        
    if(valid /= -1) then do
        let newB = play board player valid

        let winner = hasWinner newB

        if(winner == -1) then do
            start newB (round+1) positionsX positionsO
        else do
            clear
            putStrLn $ "O vencedor é: " ++ (getCharPlayer winner)
            drawBoard newB
            return();
    else do
        putStrLn $ "\nPosição inválida - porfavor digite outra\n"
        start board round positionsX positionsO

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