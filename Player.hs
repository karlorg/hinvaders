module Player where

import Graphics.UI.GLUT
import ControlState
import GeomTypes

data Player = Player { position :: Coord }

defaultPlayer :: Player
defaultPlayer = Player { Player.position = Coord (worldRight/2 - 8, 4) }

getPlayerSquares :: Player -> [Square]
getPlayerSquares Player { Player.position=Coord (left,bottom) } = let
    right = left + 16
 in [ Square { topLeft=Coord (left, bottom+6)
             , bottomRight=Coord (right,bottom) }
    , Square { topLeft=Coord (left+2,bottom+8)
             , bottomRight=Coord (right-2,bottom) }
    , Square { topLeft=Coord (left+7,bottom+10)
             , bottomRight=Coord (right-7,bottom) }
    ]

advancePlayer :: ControlState -> Player -> Player
advancePlayer cstate p = let
    Player { Player.position=Coord (x,y) } = p
    dx = 0 + (if (controlLeft cstate) then -2 else 0)
           + (if (controlRight cstate) then 2 else 0)
    x' = minimum [(maximum [0,(x+dx)]),worldRight-16]
 in p { Player.position=Coord (x',y) }
