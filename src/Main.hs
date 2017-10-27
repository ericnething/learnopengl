{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Graphics.GL
import qualified SDL.Raw as SDL
import           SDL.Raw.Enum as SDL
import Foreign
import Foreign.C
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.ByteString as BS (readFile, useAsCString)
import Data.Char (chr)

import Shader
import Util

data Game
  = Game
    { runProgram :: GLuint
    , gameValue :: GLfloat
    } deriving Show

initialGameState :: Game
initialGameState = Game 0 1.0

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
          SDLK_UP   -> acc + 0.01
          SDLK_DOWN -> acc - 0.01
          _         -> acc
        newGame v = game { gameValue = v }

draw :: SDL.Window -> Set SDL.Keysym -> Game -> IO ()
draw window keys game = do

  glClearColor 0 1 0 1
  glClear GL_COLOR_BUFFER_BIT

  -- Uniforms
  uniColor <- withCString "triangleColor" $ glGetUniformLocation (runProgram game)
  glUniform3f uniColor 0.0 0.0 (gameValue game)
  
  -- Draw
  glDrawArrays GL_TRIANGLES 0 3
  
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

initResources :: Game -> IO Game
initResources game = do

  -- VAO
  vao <- overPtr $ glGenVertexArrays 1
  glBindVertexArray vao
  
  -- VBO
  vbo <- overPtr $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  let size = fromIntegral $ length vertices * sizeOf (1.0 :: GLfloat)
  withArray vertices $ \ptr ->
    glBufferData GL_ARRAY_BUFFER size (castPtr ptr) GL_STATIC_DRAW

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
  posAttrib <- withCString "position" $ glGetAttribLocation program
  glVertexAttribPointer (fromIntegral posAttrib) 2 GL_FLOAT GL_FALSE 0 nullPtr
  glEnableVertexAttribArray (fromIntegral posAttrib)

  -- Uniforms
  uniColor <- withCString "triangleColor" $ glGetUniformLocation program
  glUniform3f uniColor 1 0 0

  return $ game { runProgram = program }


deriving instance Ord SDL.Keysym
