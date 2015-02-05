module GameState where

import Graphics.UI.GLUT
import GeomTypes
import Invader
import Player
import Shot

data GameState = GameState { invaders :: [Invader]
                           , framesToInvaderMove :: Int
                           , player :: Player
                           , shots :: [Shot]
                           }

emptyState :: GameState
emptyState = GameState { invaders = []
                       , framesToInvaderMove = 1
                       , player = defaultPlayer
                       , shots = []
                       }

initialState :: GameState
initialState = let
        invaderXs = [0,16..(realToFrac worldRight)-3*16] :: [GLfloat]
        invaderYs :: [GLfloat]
        invaderYs = [(realToFrac worldTop)-16,(realToFrac worldTop)-32
                   ..64]
        invaderCoords = [Coord (x,y) | x <- invaderXs, y <- invaderYs]
        invaders = zipWith (\c i -> i { Invader.position=c
                                      , direction=InvaderRight })
                           invaderCoords
                           (repeat defaultInvader)
     in emptyState { invaders = invaders }
