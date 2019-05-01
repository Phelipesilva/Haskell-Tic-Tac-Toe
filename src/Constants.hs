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
r1 = [x,o,o]

r2 :: Match
r2 = [o,o,x]

r3 :: Match
r3 = [x,o,x]

-- Board
board :: Board
board = [ r1,r2,r3 ]