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

verifyPositionsV2 :: Int -> [Int] -> Bool
verifyPositionsV2 n xs
    | n == head xs = True
    | length xs > 1 = verifyPositionsV2 n (tail xs)
    | otherwise = False

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

verifyPositionWinner :: [Int] -> [Int] -> String
verifyPositionWinner positions marcado
    | verifyPositionsV2 1 positions && verifyPositionsV2 4 positions && verifyPositions 7 marcado = "7"
    | verifyPositionsV2 1 positions && verifyPositionsV2 7 positions && verifyPositions 4 marcado = "4"
    | verifyPositionsV2 4 positions && verifyPositionsV2 6 positions && verifyPositions 1 marcado = "1"
    | verifyPositionsV2 2 positions && verifyPositionsV2 5 positions && verifyPositions 8 marcado = "8"
    | verifyPositionsV2 2 positions && verifyPositionsV2 8 positions && verifyPositions 5 marcado = "5"
    | verifyPositionsV2 5 positions && verifyPositionsV2 8 positions && verifyPositions 2 marcado = "2"
    | verifyPositionsV2 3 positions && verifyPositionsV2 6 positions && verifyPositions 9 marcado = "9"
    | verifyPositionsV2 3 positions && verifyPositionsV2 9 positions && verifyPositions 6 marcado = "6"
    | verifyPositionsV2 6 positions && verifyPositionsV2 9 positions && verifyPositions 3 marcado = "3"
    | verifyPositionsV2 1 positions && verifyPositionsV2 2 positions && verifyPositions 3 marcado = "3"
    | verifyPositionsV2 1 positions && verifyPositionsV2 3 positions && verifyPositions 2 marcado = "2"
    | verifyPositionsV2 2 positions && verifyPositionsV2 3 positions && verifyPositions 1 marcado = "1"
    | verifyPositionsV2 4 positions && verifyPositionsV2 5 positions && verifyPositions 6 marcado = "6"
    | verifyPositionsV2 4 positions && verifyPositionsV2 6 positions && verifyPositions 5 marcado = "5"
    | verifyPositionsV2 5 positions && verifyPositionsV2 6 positions && verifyPositions 4 marcado = "4"
    | verifyPositionsV2 7 positions && verifyPositionsV2 8 positions && verifyPositions 9 marcado = "9"
    | verifyPositionsV2 7 positions && verifyPositionsV2 9 positions && verifyPositions 8 marcado = "8"
    | verifyPositionsV2 8 positions && verifyPositionsV2 9 positions && verifyPositions 7 marcado = "7"
    | verifyPositionsV2 1 positions && verifyPositionsV2 5 positions && verifyPositions 9 marcado = "9"
    | verifyPositionsV2 1 positions && verifyPositionsV2 9 positions && verifyPositions 5 marcado = "5"
    | verifyPositionsV2 5 positions && verifyPositionsV2 9 positions && verifyPositions 1 marcado = "1"
    | verifyPositionsV2 3 positions && verifyPositionsV2 5 positions && verifyPositions 7 marcado = "7"
    | verifyPositionsV2 3 positions && verifyPositionsV2 7 positions && verifyPositions 5 marcado = "5"
    | verifyPositionsV2 5 positions && verifyPositionsV2 7 positions && verifyPositions 3 marcado = "3"
    | otherwise = "-1"

verifyCorner :: [Int]-> [Int] -> String
verifyCorner positionsX positionsO
    | (verifyPositionsV2 1 positionsO || verifyPositionsV2 7 positionsO || verifyPositionsV2 3 positionsO || verifyPositionsV2 9 positionsO) && verifyPositions 5 (positionsX++positionsO) = "5"
    | (verifyPositionsV2 1 positionsO || verifyPositionsV2 9 positionsO) &&  verifyPositions 6 (positionsX++positionsO) && verifyPositionsV2 5 positionsX = "6"
    | (verifyPositionsV2 3 positionsO || verifyPositionsV2 7 positionsO) &&  verifyPositions 4 (positionsX++positionsO) && verifyPositionsV2 5 positionsX = "4"
    | verifyPositionsV2 6 positionsO &&  verifyPositions 4 positionsO && verifyPositions 5 positionsX = "4"
    | verifyPositionsV2 4 positionsO &&  verifyPositions 6 positionsO && verifyPositions 5 positionsX = "6"
    | verifyPositionsV2 2 positionsO &&  verifyPositions 8 positionsO && verifyPositions 5 positionsX = "8"
    | verifyPositionsV2 8 positionsO &&  verifyPositions 2 positionsO && verifyPositions 5 positionsX = "2"
    | verifyPositions 1 (positionsO++positionsX) = "1"
    | verifyPositions 7 (positionsO++positionsX) = "7"
    | verifyPositions 3 (positionsO++positionsX) = "3"
    | verifyPositions 9 (positionsO++positionsX) = "9"
    | otherwise = "-1"

gerateCPUPosition :: [Int]-> [Int] -> String
gerateCPUPosition positionsX positionsO
    | (verifyPositionWinner positionsX positionsO) /= "-1" = verifyPositionWinner positionsX positionsO
    | (verifyPositionWinner positionsO positionsX) /= "-1" = verifyPositionWinner positionsO positionsX
    | (verifyCorner positionsX positionsO) /= "-1" = verifyCorner positionsX positionsO
    | otherwise = "-1"


getPlayer :: Int -> Player
getPlayer round 
    | round `mod` 2 == 0 = x
    | otherwise = o

validVerify valid round board player positionsX positionsO = do
    if(valid /= -1) then do
        let newB = play board player valid

        let winner = hasWinner newB

        if(winner == -1) then do
            if round < 9 then do
                start newB (round+1) positionsX positionsO
            else do
                putStrLn $ "Ixa deu velha"
                drawBoard newB
                return();
            
        else do
            putStrLn $ "O vencedor é: " ++ (getCharPlayer winner)
            drawBoard newB
            return();
    else do
        putStrLn $ "\nPosição inválida - porfavor digite outra\n"
        start board round positionsX positionsO


start :: Board -> Int-> [Int] -> [Int] -> IO()
start board round positionsX positionsO = do
    putStr "\n"

    let player = getPlayer round
    
    if ((mod round 2) /= 0) then do
        drawBoard board
        putStrLn "\n------------------------------"
        drawAuxBoard auxBoard
        putStrLn $ "\nVez de : " ++ (getCharPlayer player)
        putStr "> Infome a posição: "
        position <- getLine
        putStr "\n"
        let valid = (getValidPosition position (positionsX++positionsO))
        validVerify valid round board player positionsX (valid:positionsO)
        return();
    else do
        let position = gerateCPUPosition positionsX positionsO
        let valid = (getValidPosition position (positionsX++positionsO))
        validVerify valid round board player (valid:positionsX) positionsO
        return();

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