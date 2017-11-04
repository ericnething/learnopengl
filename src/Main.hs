{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Graphics.GL
import qualified SDL.Raw as SDL
import           SDL.Raw.Enum as SDL
import           Foreign
import           Foreign.C
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.ByteString as BS (readFile, useAsCString)
import           Data.Char (chr)
import           Codec.Picture (readImage, convertRGB8, Image(..))
import qualified Data.Vector.Storable as VS
import           Data.Foldable (forM_)
import           Linear

import           Shader
import           Util

data Game = Game
  { runProgram  :: GLuint
  , texture     :: GLuint
  , vao         :: GLuint
  , transformP  :: Ptr (M44 GLfloat)
  , modelP      :: Ptr (M44 GLfloat)
  , viewP       :: Ptr (M44 GLfloat)
  , projectionP :: Ptr (M44 GLfloat)
  , gameValue   :: V3 GLfloat
  , cameraPos   :: V3 GLfloat
  , cameraUp    :: V3 GLfloat
  , cameraFront :: V3 GLfloat
  , cameraSpeed :: GLfloat
  } deriving Show

initialGameState :: Game
initialGameState = Game
  { runProgram  = 0
  , texture     = 0
  , vao         = 0
  , transformP  = nullPtr
  , modelP      = nullPtr
  , viewP       = nullPtr
  , projectionP = nullPtr
  , gameValue   = V3 0 0 0
  , cameraPos   = V3 0 0 3
  , cameraUp    = V3 0 1 0
  , cameraFront = V3 0 0 (-1)
  , cameraSpeed = 0.05
  }

main :: IO ()
main = do

  SDL.init SDL_INIT_EVERYTHING

  window <- withCString "gl and sdl2 basics" $ \t ->
    SDL.createWindow t 0 0 800 600 SDL_WINDOW_SHOWN

  SDL.glSetAttribute SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_CORE
  SDL.glSetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 3
  SDL.glSetAttribute SDL_GL_CONTEXT_MINOR_VERSION 2

  SDL.glSetAttribute SDL_GL_DOUBLEBUFFER 1
  SDL.glSetAttribute SDL_GL_DEPTH_SIZE 24

  renderer <- SDL.glCreateContext window

  glEnable GL_DEPTH_TEST

  game <- initResources initialGameState
  
  loop window Set.empty game

  SDL.glDeleteContext renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Window -> Set SDL.Keysym -> Game -> IO ()
loop window keys game = do
  keys' <- parseEvents keys
  game' <- updateGame game keys'
  
  draw window keys' game'
  
  unless (Set.member escapeKey keys') $
    loop window keys' game'

updateGame :: Game -> Set SDL.Keysym -> IO Game
updateGame game keys = return . newGame $ Set.foldr check (V3 0 0 0) keys
  where check key acc = case SDL.keysymKeycode key of
          SDLK_UP    -> acc ^+^ front
          SDLK_DOWN  -> acc ^-^ front
          SDLK_RIGHT -> acc ^+^ normalize (cross front up)
          SDLK_LEFT  -> acc ^-^ normalize (cross front up)
          _          -> acc
        newGame v = game { cameraPos = cameraPos game ^+^ (speed *^ normalize v) }
        speed = cameraSpeed game
        front = cameraFront game
        up    = cameraUp    game

-- | Convert degrees to radians
toRadians = (*) (pi / 180)

draw :: SDL.Window -> Set SDL.Keysym -> Game -> IO ()
draw window keys game = do

  glClearColor 0.1 0.2 0.3 1
  glClear GL_COLOR_BUFFER_BIT
  glClear GL_DEPTH_BUFFER_BIT

  glUseProgram (runProgram game)

  -- Bind
  glBindTexture GL_TEXTURE_2D (texture game)
  glBindVertexArray (vao game)

  -- Uniforms
  model <- withCString "model" $ glGetUniformLocation (runProgram game)

  let V3 x y _   = gameValue game
      radius     = 3.0
      cameraPosition = cameraPos game
      targetPosition = cameraPosition ^+^ cameraFront game
      viewMatrix = lookAt cameraPosition targetPosition (cameraUp game)

  poke (viewP game) viewMatrix
  view <- withCString "view" $ glGetUniformLocation (runProgram game)
  glUniformMatrix4fv view 1 GL_TRUE (castPtr (viewP game))

  let projectionMatrix = perspective (toRadians 45) (800/600) 0.1 100.0
  poke (projectionP game) projectionMatrix
  projection <- withCString "projection" $ glGetUniformLocation (runProgram game)
  glUniformMatrix4fv projection 1 GL_TRUE (castPtr (projectionP game))

  -- Draw
  forM_ cubes $ \cube -> do
    let modelMatrix = mkTransformation (axisAngle (V3 1 0 0) (toRadians (-55))) cube
    poke (modelP game) modelMatrix
    glUniformMatrix4fv model 1 GL_TRUE (castPtr (modelP game))
    glDrawArrays GL_TRIANGLES 0 36

  -- Unbind
  glBindVertexArray 0
  glBindTexture GL_TEXTURE_2D 0

  SDL.glSwapWindow window

escapeKey :: SDL.Keysym
escapeKey = SDL.Keysym SDL_SCANCODE_ESCAPE SDLK_ESCAPE KMOD_NONE

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keys = do
  
  let pollEvent = alloca $ \ptr ->
        do r <- SDL.pollEvent ptr
           case r of
            0 -> return Nothing
            _ -> maybePeek peek ptr
            
  mevent <- pollEvent

  case mevent of
   Nothing -> return keys
   Just event ->

     case event of

      SDL.KeyboardEvent SDL_KEYUP _ _ _ _ k ->
        parseEvents (Set.delete k keys)

      SDL.KeyboardEvent SDL_KEYDOWN _ _ _ _ k ->
        parseEvents (Set.insert k keys)

      SDL.QuitEvent{} ->
        parseEvents (Set.insert escapeKey keys)

      _ -> parseEvents keys

square :: [GLfloat]
square =
  [ -0.5, -0.5, -0.5,  0.0, 0.0,
     0.5, -0.5, -0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5,  0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 0.0,

    -0.5, -0.5,  0.5,  0.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
    -0.5,  0.5,  0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,

    -0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5,  0.5,  1.0, 0.0,

     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5,  0.5,  0.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,

    -0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  1.0, 1.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,

    -0.5,  0.5, -0.5,  0.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5, -0.5,  0.0, 1.0
  ]

cubes :: [V3 GLfloat]
cubes =
  [ V3   0.0    0.0    0.0
  , V3   2.0    5.0  (-15.0)
  , V3 (-1.5) (-2.2) (-2.5)
  , V3 (-3.8) (-2.0) (-12.3)
  , V3   2.4  (-0.4) (-3.5)
  , V3 (-1.7)   3.0  (-7.5)
  , V3   1.3  (-2.0) (-2.5)
  , V3   1.5    2.0  (-2.5)
  , V3   1.5    0.2  (-1.5)
  , V3 (-1.3)   1.0  (-1.5)
  ]

arraySize array = fromIntegral $ length array * sizeOf (1.0 :: GLfloat)

initResources :: Game -> IO Game
initResources game = do

  -- VAO
  vao <- overPtr $ glGenVertexArrays 1
  glBindVertexArray vao
  
  -- VBO
  vbo <- overPtr $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  withArray square $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (arraySize square) (castPtr ptr) GL_STATIC_DRAW

  -- Texture
  texture <- overPtr $ glGenTextures 1
  glBindTexture GL_TEXTURE_2D texture
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  Image width height texData <- convertRGB8 <$>
                                (either (error "texture failed to load") pure =<<
                                readImage "src/textures/container.jpg")
  VS.unsafeWith texData $ \ptr ->
    glTexImage2D GL_TEXTURE_2D 0 GL_RGB (fromIntegral width) (fromIntegral height) 0 GL_RGB GL_UNSIGNED_BYTE (castPtr ptr)
  glGenerateMipmap GL_TEXTURE_2D
  glBindTexture GL_TEXTURE_2D 0

  -- Vertex Shader
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  compileShader vertexShader "src/shaders/triangle.vertex.glsl"

  -- Fragment Shader
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  compileShader fragmentShader "src/shaders/triangle.fragment.glsl"

  -- Shader Program
  program <- createProgram vertexShader fragmentShader
  glUseProgram program

  -- Link Vertex data with Attributes
  let floatSize = sizeOf (1.0 :: GLfloat)
  posAttrib <- withCString "position" $ glGetAttribLocation program
  glVertexAttribPointer (fromIntegral posAttrib) 3 GL_FLOAT GL_FALSE (fromIntegral $ 5 * floatSize) nullPtr
  glEnableVertexAttribArray (fromIntegral posAttrib)

  -- Link Texture data with Attributes
  textureAttrib <- withCString "texCoord" $ glGetAttribLocation program
  glVertexAttribPointer (fromIntegral textureAttrib) 2 GL_FLOAT GL_FALSE (fromIntegral $ 5 * floatSize) (plusPtr nullPtr (3 * floatSize))
  glEnableVertexAttribArray (fromIntegral textureAttrib)

  -- Transformation matrix pointer
  transformP <- malloc
  modelP <- malloc
  viewP <- malloc
  projectionP <- malloc

  return $ game
    { runProgram = program
    , texture = texture
    , vao = vao
    , transformP = transformP
    , modelP = modelP
    , viewP = viewP
    , projectionP = projectionP
    }


deriving instance Ord SDL.Keysym
