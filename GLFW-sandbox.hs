module Main where

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO

import           Data.Array.Storable       (newListArray, withStorableArray)
import qualified Data.ByteString           as S8
import           Foreign.Ptr               (nullPtr)
import           Foreign.Storable          (sizeOf)
import System.Random
import Control.Parallel.Strategies
import Control.Parallel

-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit easier to read

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description

keyCallback :: G.KeyCallback
keyCallback window key scancode action mods = when (key == G.Key'Escape && action == G.KeyState'Pressed) $
  G.setWindowShouldClose window True
  
main :: IO ()
main = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          G.setKeyCallback window (Just keyCallback)
          --GL.arrayPointer GL.
          --glGenVertexArrays(&vao)
          [vertexArrayId] <- GL.genObjectNames 1 :: IO [VertexArrayObject]
          --glBindVertexArray(vao)
          GL.bindVertexArrayObject $= Just vertexArrayId
          vbo <- createVBO vertices
          program <- loadShaders "./vertexShader.glsl" "./fragmentShader.glsl"
          GL.currentProgram $= Just program
          pState <- get $ GL.validateStatus program
          unless pState $ putStrLn "Shader validate Failed"
          ts <- sequence $ map (\a -> randTris) [1..10]
          mainLoop window vbo vertexArrayId ts
          G.destroyWindow window
          G.terminate
          exitSuccess

rand n = newStdGen >>= return . (* n) . fst . randomR (0,1)

randTris = do
    v <- rand 1 :: IO GLdouble
    x <- rand 1 :: IO GLfloat
    y <- rand 1 :: IO GLfloat
    z <- rand 1 :: IO GLfloat
    vx <- rand 360 :: IO GLdouble
    vy <- rand 360 :: IO GLdouble
    vz <- rand 360 :: IO GLdouble
    return ((1 + v), vx, vy, vz, vx, vy, vz, (x - 0.5), (y - 0.5), z)

vertices :: [GLfloat]
vertices = [-0.5, -0.5, 0.0
           ,0.5, -0.5, 0.0
           ,0.5, 0.5, 0.0
           ,-0.5, 0.5, 0.0]


createVBO :: [GLfloat] -> IO BufferObject
createVBO elems = do
  [vertexBuffer] <- GL.genObjectNames 1
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  arr <- newListArray (0,len-1)elems
  let bufSize = toEnum $ len * sizeOf (head elems)
  withStorableArray arr $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (bufSize,ptr,GL.StaticDraw)
  putStrLn "array buffered"
  GL.bindBuffer GL.ArrayBuffer $= Nothing -- なくてもいい？
  print "buffer binded"
  return vertexBuffer
  where
    len = length elems

draw :: BufferObject -> VertexArrayObject -> IO ()
draw vertexBuffer vao = do
  

  --glDrawArraysを書く
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer               $= Just vertexBuffer
  GL.bindVertexArrayObject $= Just vao
  GL.vertexAttribPointer (GL.AttribLocation 0)$= (GL.ToFloat,descriptor)
  GL.drawArrays GL.Quads 0 4
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled

  GL.flush
  where
    descriptor= GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr
      :: GL.VertexArrayDescriptor GLfloat

loadShaders :: FilePath -> FilePath -> IO GL.Program
loadShaders vertexFilePath fragmentFilePath = do

  vertexShaderId   <- GL.createShader GL.VertexShader
  vShaderData <- S8.readFile vertexFilePath
  putStrLn $ "compilingShader : " ++ vertexFilePath
  GL.shaderSourceBS vertexShaderId $= vShaderData
  GL.compileShader vertexShaderId

  vShaderLog <- get $ GL.shaderInfoLog vertexShaderId
  putStrLn vShaderLog

  fragmentShaderId <- GL.createShader GL.FragmentShader
  fShaderData <- S8.readFile fragmentFilePath
  putStrLn $ "compilingShader : " ++ fragmentFilePath
  GL.shaderSourceBS fragmentShaderId $= fShaderData
  GL.compileShader fragmentShaderId
  fShaderLog <- get $ GL.shaderInfoLog fragmentShaderId
  putStrLn $ "shader log :\n" ++ fShaderLog

  --link Program
  putStrLn "Linking Shader Program"
  programId <- GL.createProgram
  GL.attachShader programId vertexShaderId
  -- GL.programID vertexShaderId
  GL.attachShader programId fragmentShaderId
  GL.linkProgram programId

  --check program
  programLog <- get $ GL.programInfoLog programId
  putStrLn $ "program link log :\n" ++ programLog

  GL.deleteObjectName vertexShaderId
  GL.deleteObjectName fragmentShaderId
  return programId

drawTris vb vao (v, ox, oy, oz, vx, vy, vz, x, y, z) = do
    loadIdentity
    translate (Vector3 x y z :: Vector3 GLfloat)
    rotate vx (Vector3 1 0 0 :: Vector3 GLdouble)
    rotate vy (Vector3 0 1 0 :: Vector3 GLdouble)
    rotate vz (Vector3 0 0 1 :: Vector3 GLdouble)
    draw vb vao

updateTris tt ts = pmap' (\(v, ox, oy, oz, vx, vy, vz, x, y, z) -> do
                             let t = (360.0 :: GLdouble) * ((realToFrac $ sin (v * tt)) / 4)
                             (v, ox, oy, oz, (t + ox), (t + oy), (t + oz), x, y, z)) ts
              
pmap' fn (a:b:[]) = let c = fn a in pseq c (c : (fn b) : [])
pmap' fn (a:ax) = let c = fn a in par c (c : pmap' fn ax)

-- mainLoop :: G.Window -> IO ()
mainLoop w vb vao tris = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height
    
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    -- clear [ColorBuffer]
    
    matrixMode $= Projection
    loadIdentity
    -- ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    frustum (negate ratio) ratio (negate 1.0) 1.0 0.0 20
    matrixMode $= Modelview 0
    loadIdentity
    -- this is bad, but keeps the logic of the original example I guess
    -- translate $ (Vector3 0 0 0 :: Vector3 GLfloat)
    GL.clear [GL.ColorBuffer,GL.DepthBuffer]
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.polygonMode $= (GL.Line, GL.Line)
    GL.multisample $= GL.Enabled
    GL.lineWidth $= 1.5
    GL.lineSmooth $= GL.Enabled
    G.swapInterval 1
    Just t <- G.getTime
    mapM_ (drawTris vb vao) tris
  --  renderPrimitive Triangles $ do
  --      color  (Color3 1 0 0 :: Color3 GLdouble)
  --      vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
  --      color  (Color3 0 1 0 :: Color3 GLdouble)
  --      vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
  --      color  (Color3 0 0 1 :: Color3 GLdouble)
  --      vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
        
    G.swapBuffers w
    G.pollEvents
    mainLoop w vb vao $ updateTris (realToFrac t) tris
