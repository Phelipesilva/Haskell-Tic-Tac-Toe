-- Constants 
module Constants where

-- Imports
import Types

-- Players
players :: Players 
players = (1, 0)

empty :: Empty
empty = (-1)

-- X (1)
x :: Player
x = fst(players)

-- O (0)
o :: Player
o = snd(players)

-- Rows
r1 :: Match
r1 = [empty, empty, empty]

r2 :: Match
r2 = [empty, empty, empty]

r3 :: Match
r3 = [empty, empty, empty]

-- Board
board :: Board
board = [ r1, r2, r3 ]