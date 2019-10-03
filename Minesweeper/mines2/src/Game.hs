
module Game where

import qualified Data.Map.Strict as SM
import System.Random
import Data.Maybe
import Graphics.UI.GLUT

import Input

type Flagged = Bool
type MineCount = Int
type ClickedMinePos = (Int, Int)
data GameState = InitialisedGS | PlayingGS | GameOverGS ClickedMinePos deriving (Eq, Ord, Read, Show)
data TileState = CoveredTS Flagged | MineFreeTS MineCount | MineTS deriving (Eq, Ord, Read, Show)

data Game = Game { rndGen :: StdGen
                 , gameState :: GameState
                 , boardSize :: (Int, Int)
                 , board :: SM.Map (Int, Int) TileState
                 , minePositions :: SM.Map (Int, Int) Bool -- The last version of the Hashset library has an issue resulting in the application crashing under window. Until this is fixed, I am using a Map in place of a Hashset. The Bool value in the map will always be ignored.
                 , viewportRes :: (GLsizei, GLsizei)
                 , bitmapAtlas :: TextureObject
                 }

defaultMineCount :: Int
defaultMineCount = 150

defaultBoardSize :: (Int, Int)
defaultBoardSize = (40, 20)

-- Reiniciar el juego.
restartGame :: Game -> Game
restartGame g = g { board = board'
                  , gameState = InitialisedGS
                  , minePositions = SM.empty
                  }
    where (boardW, boardH) = boardSize g
          positions = [(x, y) | x <- [0..boardW-1], y <- [0..boardH-1]]
          board' = SM.fromList $ zip positions (repeat $ CoveredTS False)

-- Actualizar el juego según los inputs.
updateGame :: InputData -> Game -> Game
updateGame input game = game'
    where 
          -- ****************************** Obtención entradas ******************************
          mousePosition = mouseBoardPos (mousePos input) game
          mouseLeftPressed = elem (MouseButton LeftButton) (onKeys input)
          mouseRightPressed = elem (MouseButton RightButton) (onKeys input)

          -- ****************************** Actualizaciones del juego ******************************
          game' | mouseLeftPressed = case gameState game of
                                          InitialisedGS -> (revealTile mousePosition) . (placeMines mousePosition defaultMineCount) $ game {gameState = PlayingGS}
                                          PlayingGS -> case board game SM.! mousePosition of
                                                            CoveredTS _ -> case SM.lookup mousePosition $ minePositions game of
                                                                                Nothing -> revealTile mousePosition game
                                                                                Just _  -> revealBombs $ game {gameState = GameOverGS mousePosition}
                                                            _           -> game
                                          GameOverGS _ -> restartGame game

                | mouseRightPressed = case gameState game of
                                           PlayingGS -> case board game SM.! mousePosition of
                                                             CoveredTS flagged -> let board' = SM.insert mousePosition (CoveredTS (not flagged)) $ board game
                                                                                  in game {board = board'}
                                                             _                 -> game
                                           _         -> game

                | otherwise = game

-- Ubicar todas las minas después de un clic inicial
placeMines :: (Int, Int) -> Int -> Game -> Game
placeMines (clickX, clickY) mineCount game =
    let (boardW, boardH) = boardSize game
        maxMineCount = floor $ (fromIntegral boardW) * (fromIntegral boardH) * 0.75
        placeMines' 0         rndGen' placedMines = game {rndGen = rndGen', minePositions = placedMines}
        placeMines' mineCount rndGen' placedMines = let (mineX, rndGen'') = randomR (0, boardW-1) rndGen'
                                                        (mineY, rndGen''') = randomR (0, boardH-1) rndGen''
                                                        mineTooCloseToFirstClick = abs (clickX - mineX) <= 1 &&
                                                                                   abs (clickY - mineY) <= 1
                                                        alreadyAMine = SM.member (mineX, mineY) placedMines
                                                        placedMines' = SM.insert (mineX, mineY) False placedMines
                                                    in if alreadyAMine || mineTooCloseToFirstClick 
                                                         then placeMines' mineCount rndGen''' placedMines
                                                         else placeMines' (mineCount-1) rndGen''' placedMines'
    in placeMines' (max 0 $ min maxMineCount mineCount) (rndGen game) SM.empty

-- Revela la celda en la posición dada y actualiza el juego consequentemente.
revealTile :: (Int, Int) -> Game -> Game
revealTile clickedPos game =
    case SM.lookup clickedPos $ minePositions game of
         Nothing -> let (clickX, clickY) = clickedPos
                        (boardW, boardH) = boardSize game
                        surrounding = [(x,y) | x <- [clickX-1 .. clickX+1], x >= 0, x < boardW
                                             , y <- [clickY-1 .. clickY+1], y >= 0, y < boardH
                                             , (x,y) /= clickedPos]
                        surroundingMineCount = length $ filter (\pos -> SM.member pos $ minePositions game) $ surrounding
                        isCovered tileState = case tileState of
                                                   CoveredTS _ -> True
                                                   _           -> False
                        surroundingToVisit = filter (\pos -> isCovered $ board game SM.! pos) surrounding
                        board' = SM.insert clickedPos (MineFreeTS surroundingMineCount) $ board game
                        game' = game {board = board'}
                    in case surroundingMineCount of
                            0 -> foldr revealTile game' surroundingToVisit
                            n -> game'
         Just _  -> let board' = SM.insert clickedPos MineTS $ board game
                    in game {board = board'}

-- Revela todas las bombas.
revealBombs :: Game -> Game
revealBombs game = foldr (\pos g -> revealTile pos g) game $ SM.keys $ minePositions game
         
-- Convierte las coordenadas de la ventana en las coordenadas del tablero de juego.
mouseBoardPos :: Position -> Game -> (Int, Int)
mouseBoardPos (Position mX mY) game = let (resW, resH) = viewportRes game
                                          (boardW, boardH) = boardSize game
                                      in (floor $ (fromIntegral mX / fromIntegral resW) * fromIntegral boardW,
                                          floor $ (fromIntegral mY / fromIntegral resH) * fromIntegral boardH)
