module Main where

import qualified Data.Map.Strict as SM
import System.Random
import Data.Maybe
import Data.IORef
import Graphics.UI.GLUT
import Control.Concurrent

import Input
import Render
import OpenGLUtils
import Game


main = do
    -- Inicialización de la ventana
    let viewportPos = (Position 0 0)
        windowSize = Size (20*40) (20*20)

    (_progName, _args) <- getArgsAndInitialize
    initialWindowSize $= windowSize
    initialDisplayMode $= [WithDepthBuffer]
    window <- createWindow $ "Haskell Project Minesweeper"
    viewport $= (viewportPos, windowSize)

    -- Texturas
    bitmapAtlas' <- loadGLTextureFromFile "Images/atlas.png"

    -- Inicialización del game & input
    rndGen' <- getStdGen
    gameIORef <- newIORef $ restartGame $ Game { rndGen = rndGen'
                                               , gameState = InitialisedGS
                                               , boardSize = defaultBoardSize
                                               , board = SM.empty
                                               , minePositions = SM.empty
                                               , viewportRes = (20*40, 20*20)
                                               , bitmapAtlas = bitmapAtlas'
                                               }
    inputState <- newIORef newInputData

    -- Configuración del OpenGL
    depthFunc $= Just Lequal
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    normalize $= Enabled
    shadeModel $= Smooth
    texture Texture2D $= Enabled

    -- Callbacks
    idleCallback $= Just (do
                            updateGameIO inputState gameIORef
                            postRedisplay Nothing
                            threadDelay 5000)
    keyboardMouseCallback $= Just (\k kState kModif mPos -> do
                                                              mouseKeyboardCallback inputState k kState kModif mPos
                                                              updateGameIO inputState gameIORef
                                                              postRedisplay Nothing)
    displayCallback $= display gameIORef
    motionCallback $= Just (\newMousePos -> do
                                              mouseMovementCallback inputState newMousePos
                                              updateGameIO inputState gameIORef
                                              postRedisplay Nothing)
    reshapeCallback $= Just (\(Size x' y') -> do
                                                Graphics.UI.GLUT.windowSize $= Size x' y'
                                                viewport $= ((Position 0 0), (Size x' y'))
                                                modifyIORef' gameIORef (\g -> g {viewportRes = (x', y')}))
    passiveMotionCallback $= Just (\newMousePos -> do
                                                     mouseMovementCallback inputState newMousePos
                                                     updateGameIO inputState gameIORef
                                                     postRedisplay Nothing)

    -- Primer display & Main loop
    postRedisplay Nothing

    mainLoop

updateGameIO :: IORef InputData -> IORef Game -> IO()
updateGameIO keysIORef gameIORef = do
    game <- readIORef gameIORef
    input <- readIORef keysIORef

    modifyIORef' gameIORef $ updateGame input

    modifyIORef' keysIORef (\x -> cancelOnKeys x)
