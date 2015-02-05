module Shot where

import GeomTypes

data Shot = Shot { position :: Coord } deriving (Eq, Show)

advanceShot :: Shot -> Maybe Shot
advanceShot s@Shot { Shot.position=Coord (x,y) } =
    if y < worldTop
       then Just s { Shot.position=Coord (x,y+4) }
       else Nothing

shotSquare :: Shot -> Square
shotSquare Shot { Shot.position=Coord (left,bottom) } =
    Square { topLeft=Coord (left,bottom+8)
           , bottomRight=Coord (left+2,bottom) }
