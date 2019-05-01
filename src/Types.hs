-- Types 
module Types where

-- Player: O (1) or X (0)
type Player = Int

-- Empty: (-1)
type Empty = Int

-- Value: Player or Empty
type Value = Int

-- Players: O (1) and X (0)
type Players = ( Player, Player )

-- Match: Group with three position
type Match = [ Value ]

-- Board: Group with three matchs
type Board = [ Match ]

{--
    Board Positions
    
    [1 2 3] ,
    [4 5 6] ,
    [7 8 9]

    Winners:

    Row
    1 , 2 , 3
    4 , 5 , 6
    7 , 8 , 9

    Column
    1 , 4 , 7
    2 , 5 , 8
    3 , 6 , 9

    Diagonal
    1 , 5 , 9
    3 , 5 , 7
--}