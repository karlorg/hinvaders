import Graphics.UI.GLUT
import Data.IORef
import Data.List ((\\))
import Data.Maybe (mapMaybe)

import ControlState
import Drawing
import GameState
import GeomTypes
import Invader
import Player
import Shot

main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 800 600
    initialWindowPosition $= Position 0 0
    _window <- createWindow "Hinvaders"
    state <- newIORef initialState
    cstate <- newIORef initialControlState
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse cstate)
    displayCallback $= display state
    idleCallback $= Nothing
    addTimerCallback 100 (timerCallback cstate state)
    mainLoop

reshape :: ReshapeCallback
reshape size = do 
    viewport $= ( Position 0 0, size )

display :: IORef GameState -> DisplayCallback
display state = do 
    clear [ColorBuffer]
    loadIdentity
    ortho2D 0 (realToFrac worldRight) 0 (realToFrac worldTop)
    state' <- get state
    let is = invaders state'
    color $ Color3 1.0 1.0 (1.0 :: GLfloat)
    mapM_ (drawSquare . invaderSquare) is
    let p = player state'
    mapM_ drawSquare $ getPlayerSquares p
    let ss = shots state'
    mapM_ (drawSquare . shotSquare) ss
    swapBuffers

timerCallback :: IORef ControlState -> IORef GameState -> IdleCallback
timerCallback cstateref stateref = do
    addTimerCallback (1000 `div` 30) (timerCallback cstateref stateref)
    state <- get stateref
    cstate <- get cstateref
    let state' = update cstate state
    writeIORef stateref state'
    postRedisplay Nothing

update :: ControlState -> GameState -> GameState
update cstate state = let
    is = invaders state
    framesToInvMove = (framesToInvaderMove state) - 1
    is' = if framesToInvMove < 1
             then advanceInvaders is
             else is
    framesToInvMove' = if framesToInvMove < 1
                          then frameDelayForInvaderCount $ length is'
                          else framesToInvMove - 1
    p = player state
    p' = advancePlayer cstate p
    ss = shots state
    ss' = ((maybeFire cstate p') . (mapMaybe advanceShot)) ss
    collisions = mapMaybe (maybeShotHit is') ss'
    is'' = is' \\ map fst collisions
    ss'' = ss' \\ map snd collisions
 in state { invaders = is''
          , framesToInvaderMove = framesToInvMove'
          , player = p'
          , shots = ss''
          }

keyboardMouse :: IORef ControlState -> KeyboardMouseCallback
keyboardMouse cstateref key keyState _ _ = do
    let controlBool = case keyState of Down -> True
                                       Up -> False
    cstate <- get cstateref
    let cstate' =
            case key of
                SpecialKey KeyLeft -> cstate { controlLeft = controlBool }
                SpecialKey KeyRight -> cstate { controlRight = controlBool }
                Char ' ' -> cstate { controlFire = controlBool }
                otherwise -> cstate
    writeIORef cstateref cstate'

maybeFire :: ControlState -> Player -> [Shot] -> [Shot]
maybeFire cstate p ss = let
    Player { Player.position=Coord (px,py) } = p
 in if length ss > 0 || controlFire cstate == False
       then ss
       else [Shot { Shot.position=Coord(px+7,py+8) }]

maybeShotHit :: [Invader] -> Shot -> Maybe (Invader,Shot)
maybeShotHit is shot = let
    foldFunc (Just x) _ = Just x
    foldFunc Nothing i = if collideSquares (invaderSquare i)
                                           (shotSquare shot)
                            then Just (i,shot)
                            else Nothing
 in foldl foldFunc Nothing is
