module Invader where

import GeomTypes

data Invader = Invader { position :: Coord
                       , direction :: InvaderDirection
                       } deriving (Eq, Show)

data InvaderDirection = InvaderLeft | InvaderRight deriving (Eq, Show)

defaultInvader :: Invader
defaultInvader = Invader { position = Coord (0,0)
                         , direction = InvaderRight
                         }

invaderSquare :: Invader -> Square
invaderSquare Invader { position=Coord (ix,iy) } =
    Square { topLeft = Coord (ix,iy+8)
           , bottomRight = Coord (ix+8,iy)
           }

frameDelayForInvaderCount :: Int -> Int
frameDelayForInvaderCount n
  | n == 1 = 0
  | n < 10 = 1
  | n < 30 = 3
  | n < 70 = 10
  | otherwise = 20

advanceInvaders :: [Invader] -> [Invader]
advanceInvaders = moveInvaders . dropIfNecessary

dropIfNecessary :: [Invader] -> [Invader]
dropIfNecessary is
  | atEdge is = map (reverseDirection . dropDown) is
  | otherwise = is

atEdge :: [Invader] -> Bool
atEdge [] = False
atEdge is@(Invader { direction=d }:_) = let
    getX Invader { position=Coord (x,_) } = x
    atLeft = (<1) . minimum . map getX
    atRight = (>=worldRight-8) . maximum . map getX
 in case d of
         InvaderLeft -> atLeft is
         InvaderRight -> atRight is

moveInvaders :: [Invader] -> [Invader]
moveInvaders = map moveInvader

moveInvader :: Invader -> Invader
moveInvader i@Invader { position=Coord (x,y), direction=d } = let
    x' = if d == InvaderLeft
            then x - 1
            else x + 1
 in i { Invader.position=Coord (x',y) }

dropDown :: Invader -> Invader
dropDown i@Invader { position=Coord (x,y) } =
    i { Invader.position=Coord (x,y-8) }

reverseDirection :: Invader -> Invader
reverseDirection i@Invader { direction=d } = let
    d' = if d == InvaderLeft then InvaderRight else InvaderLeft
 in i { direction = d' }
