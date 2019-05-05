import Constants
import Functions
import Interface
 
main = do
    putStrLn "-- Bem vindo ao HaskellTicTacToe --"
    putStrLn "------------------------------"
    drawAuxBoard auxBoard
    putStrLn "------------------------------"
    start board 0
    return()

