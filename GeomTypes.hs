module GeomTypes where

import Graphics.UI.GLUT

worldRight = 240 :: GLfloat
worldTop = 180 :: GLfloat

newtype Coord = Coord (GLfloat,GLfloat) deriving (Eq, Show)

data Square = Square { topLeft :: Coord, bottomRight :: Coord }

collideSquares :: Square -> Square -> Bool 
collideSquares s0 s1 = let
    Square { topLeft=Coord (left0,top0)
           , bottomRight=Coord (right0,bottom0) } = s0
    Square { topLeft=Coord (left1,top1)
           , bottomRight=Coord (right1,bottom1) } = s1
    collide
      | right1 < left0 = False
      | right0 < left1 = False
      | top1 < bottom0 = False
      | top0 < bottom1 = False
      | otherwise = True
 in collide
