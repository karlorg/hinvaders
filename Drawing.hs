module Drawing (
    drawSquare
) where

import Graphics.UI.GLUT
import GeomTypes

vertex2i :: (GLint, GLint) -> IO ()
vertex2i (x, y) = vertex $ Vertex2 x y

fromIntegralPair :: (Int, Int) -> (GLint, GLint)
fromIntegralPair (x,y) = (fromIntegral x, fromIntegral y)

drawSquare :: Square -> IO ()
drawSquare square = do
    let Coord (left,top) = topLeft square
        Coord (right,bottom) = bottomRight square
    preservingMatrix $ do
        translate $ Vector3 left bottom (0 :: GLfloat)
        scale (right-left) (top-bottom) (1.0 :: GLfloat)
        renderPrimitive Quads $ mapM_ (vertex2i . fromIntegralPair)
            [ (0,0), (1,0), (1,1), (0,1) ]
