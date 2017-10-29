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
import           Linear

import           Shader
import           Util

data Game = Game
  { runProgram :: GLuint
  , texture :: GLuint
  , vao :: GLuint
  , transformP :: Ptr (M44 GLfloat)
  , gameValue :: V3 GLfloat
  } deriving Show

initialGameState :: Game
initialGameState = Game 0 0 0 nullPtr (V3 0.0 0.0 0.0)

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
updateGame game keys = return . newGame $ Set.foldr check (gameValue game) keys
  where check key acc = case SDL.keysymKeycode key of
          SDLK_UP    -> acc ^+^ V3 0.0  0.01 0.0
          SDLK_DOWN  -> acc ^-^ V3 0.0  0.01 0.0
          SDLK_RIGHT -> acc ^+^ V3 0.01 0.0  0.0
          SDLK_LEFT  -> acc ^-^ V3 0.01 0.0  0.0
          _          -> acc
        newGame v = game { gameValue = v }

draw :: SDL.Window -> Set SDL.Keysym -> Game -> IO ()
draw window keys game = do

  glClearColor 0.1 0.2 0.3 1
  glClear GL_COLOR_BUFFER_BIT

  glUseProgram (runProgram game)

  -- Bind
  glBindTexture GL_TEXTURE_2D (texture game)
  glBindVertexArray (vao game)

  -- Uniforms
  let transformMatrix = mkTransformationMat identity (gameValue game)
  poke (transformP game) (transpose transformMatrix)
  transform <- withCString "transform" $ glGetUniformLocation (runProgram game)
  glUniformMatrix4fv transform 1 GL_FALSE (castPtr (transformP game))
  
  -- Draw
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr

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

vertices :: [GLfloat]
vertices =
  [  0.0,  0.5
  ,  0.5, -0.5
  , -0.5, -0.5
  ]

square :: [GLfloat]
square =
  -- position     colors     textures
  [ -0.5,  0.5,   1, 1, 0,   0, 1   -- left  top
  ,  0.5,  0.5,   1, 0, 0,   1, 1   -- right top
  ,  0.5, -0.5,   0, 1, 0,   1, 0   -- right bottom
  , -0.5, -0.5,   0, 0, 1,   0, 0   -- left  bottom
  ]

squareIndices :: [GLuint]
squareIndices =
  [ 0, 1, 2
  , 2, 3, 0
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

  -- EBO
  ebo <- overPtr $ glGenBuffers 1
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  withArray squareIndices $ \ptr ->
    glBufferData GL_ELEMENT_ARRAY_BUFFER (arraySize squareIndices) (castPtr ptr) GL_STATIC_DRAW

  -- Texture
  texture <- overPtr $ glGenTextures 1
  glBindTexture GL_TEXTURE_2D texture
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
  glVertexAttribPointer (fromIntegral posAttrib) 2 GL_FLOAT GL_FALSE (fromIntegral $ 7 * floatSize) nullPtr
  glEnableVertexAttribArray (fromIntegral posAttrib)

  -- Link Texture data with Attributes
  textureAttrib <- withCString "texCoord" $ glGetAttribLocation program
  glVertexAttribPointer (fromIntegral textureAttrib) 2 GL_FLOAT GL_FALSE (fromIntegral $ 7 * floatSize) (plusPtr nullPtr (5 * floatSize))
  glEnableVertexAttribArray (fromIntegral textureAttrib)

  -- Link Color data with Attributes
  colorAttrib <- withCString "inColor" $ glGetAttribLocation program
  glVertexAttribPointer (fromIntegral colorAttrib) 3 GL_FLOAT GL_FALSE (fromIntegral $ 7 * floatSize) (plusPtr nullPtr (2 * floatSize))
  glEnableVertexAttribArray (fromIntegral colorAttrib)

  -- Transformation matrix pointer
  transformP <- malloc

  return $ game
    { runProgram = program
    , texture = texture
    , vao = vao
    , transformP = transformP
    }


deriving instance Ord SDL.Keysym
