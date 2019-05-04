import Constants
import Functions
import Types
import Interface
 
main = do
    putStrLn "-- Bem vindo ao HaskellTicTacToe --"
    putStrLn "------------------------------"
    drawAuxBoard auxBoard
    putStrLn "------------------------------"
    start board 0
    return()

getPlayer :: Int -> Player
getPlayer round 
    | round `mod` 2 == 0 = x
    | otherwise = o

start :: Board -> Int -> IO()
start board round = do    
    clear
    drawBoard board
    let player = getPlayer round
    putStrLn $ "Vez de : " ++ (getCharPlayer player)

    putStr "Infome a posição: "
    position <- getLine

    let valid = (getValidPosition position)
    let newB = play board player valid

    let winner = hasWinner newB

    if(winner == -1) then do
        start newB (round+1)
    else do
        clear
        putStrLn $ "O vencedor é: " ++ (getCharPlayer winner)
        drawBoard newB
        return();