module Render where

import qualified Data.Map.Strict as SM
import Data.IORef
import Graphics.UI.GLUT
import Control.Monad

import Game
import OpenGLUtils

data AtlasUV = CoveredUV | UncoveredUV | FlagUV | MineUV | DigitUV Int deriving (Eq, Ord, Read, Show)

display :: IORef Game -> DisplayCallback
display gameIORef = do
  game <- readIORef gameIORef

  clear [ColorBuffer, DepthBuffer]
  mapM_ (drawTile game) $ SM.assocs $ board game
  color $ Color3 1 1 (1 :: GLfloat)
  flush

drawTile :: Game -> ((Int, Int), TileState) -> IO()
drawTile game ((x,y), tileState) = do
    let (boardW, boardH) = boardSize game
        (tileW, tileH) = (2 / fromIntegral boardW,
                          2 / fromIntegral boardH)
        x' =  ((fromIntegral x / fromIntegral boardW) - 0.5) * 2
        y' = -((fromIntegral y / fromIntegral boardH) - 0.5) * 2
        drawTexture' uv = drawTexture uv (1/4, 1/4) (x',y') (tileW, tileH)
    case tileState of
         CoveredTS True -> do drawTexture' $ imageUV CoveredUV
                              drawTexture' $ imageUV FlagUV

         CoveredTS _    -> do drawTexture' $ imageUV CoveredUV

         MineFreeTS mc  -> do drawTexture' $ imageUV UncoveredUV
                              when (mc > 0) $ do
                                drawTexture' $ imageUV (DigitUV mc)

         MineTS         -> do drawTexture' $ imageUV CoveredUV
                              drawTexture' $ imageUV MineUV
         -- ts             -> error $ "Unkown TileState: " ++ (show ts)

imageUV :: AtlasUV -> (GLfloat, GLfloat)
imageUV CoveredUV        = (1/4, -2/4)
imageUV UncoveredUV      = (2/4, -2/4)
imageUV FlagUV           = (0/4, -3/4)
imageUV MineUV           = (1/4, -3/4)
imageUV (DigitUV n)      = let u = fromIntegral (mod (n-1) 4) / 4
                               v = -(fromIntegral $ div (n-1) 4)
                           in (u, v)
-- imageUV atlasUV          = error $ "Unknown AtlasUV: " ++ (show atlasUV)

