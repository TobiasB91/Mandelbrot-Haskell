import Graphics.UI.GLUT
import Data.Complex
import Data.StateVar
import Data.IORef
import Prelude hiding (iterate) 

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialWindowSize $= Size 1024 768
    _window <- createWindow "Mandelbrot Zoomer"
    borders <- newIORef ((-2.0, 0.5, 1.25, -1.25) :: (GLfloat, GLfloat, GLfloat, GLfloat)) 
    displayCallback $= (display borders)
    keyboardMouseCallback $= Just (keyboard borders)
    mainLoop


display :: IORef (GLfloat, GLfloat, GLfloat, GLfloat) ->  DisplayCallback
display borders = do
    pt@(l,r,t,b) <- readIORef borders
    clear [ ColorBuffer ]
    matrixMode $= Projection
    loadIdentity
    ortho (realToFrac l) (realToFrac r) (realToFrac b) (realToFrac t) (-1.0) (1.0)
    matrixMode $= Modelview 0
    loadIdentity
    renderPrimitive Points $ mapM_ (\p@(x, y, z) -> do
        currentColor $= (calcColor $ iterate $ pixelToComplex p)
        vertex $ Vertex3 x y z) $ pixels pt
    flush

keyboard :: IORef (GLfloat, GLfloat, GLfloat, GLfloat) -> KeyboardMouseCallback
keyboard borders key Down _ _ = do
    (l,r,t,b) <- readIORef borders
    let cX = (l+r)*0.5 
    let cY = (t+b)*0.5
    let w = r-l
    let h = t-b
    let zoom = 0.2 
    let span = 0.06
    case key of 
        (Char '+') -> do
            let nL = cX - 0.5 * w /  (1.0 + zoom)  
            let nR = cX + 0.5 * w / (1.0 + zoom)
            let nT = cY + 0.5 * h / (1.0 + zoom)
            let nB = cY - 0.5 * h / (1.0 + zoom)
            borders $= (nL, nR, nT, nB)
        (Char '-') -> do
            let nL = cX - (1.0 + zoom) * 0.5 * w
            let nR = cX + (1.0 + zoom) * 0.5 * w
            let nT = cY + (1.0 + zoom) * 0.5 * h
            let nB = cY - (1.0 + zoom) * 0.5 * h
            borders $= (nL, nR, nT, nB)
        (Char 'a') -> do
            let nL = l - span * w
            let nR = r - span * w
            borders $= (nL, nR, t, b)
        (Char 'd') -> do
            let nL = l + span * w
            let nR = r + span * w
            borders $= (nL, nR, t, b)
        (Char 's') -> do
            let nT = t - span * h
            let nB = b - span * h
            borders $= (l, r, nT, nB)
        (Char 'w') -> do
            let nT = t + span * h
            let nB = b + span * h
            borders $= (l, r, nT, nB)
        _ -> return ()
    postRedisplay Nothing
keyboard _ _ _ _ _ = return () 

pixels :: (GLfloat, GLfloat, GLfloat, GLfloat) -> [ (GLfloat, GLfloat, GLfloat) ]
pixels (l,r,t,b) = [ (l + (r-l) * (x/1023), t + (b-t) * (y/767) , 0) | x <- [0..1024], y <- [0..768]  ]

pixelToComplex :: (GLfloat, GLfloat, GLfloat) -> Complex GLfloat
pixelToComplex (x,y,_) = x :+ (-y)

nextMandelbrot :: Complex GLfloat -> Complex GLfloat -> Complex GLfloat
nextMandelbrot c z = z * z + c

iterate :: Complex GLfloat -> Int
iterate c = iterate' (0.0 :+ 0.0) c 0

iterate' :: Complex GLfloat -> Complex GLfloat -> Int -> Int
iterate' z c n | n >= 100 = 100
               | otherwise = let z' = nextMandelbrot c z 
                                 x = realPart z'
                                 y = imagPart z'
                             in if (x * x) + (y * y) > 4
                                then n
                                else iterate' z' c $ n + 1 

calcColor :: Int -> Color4 GLfloat
calcColor iter = let g = fromIntegral iter / 255 in Color4 g g 1 1 
