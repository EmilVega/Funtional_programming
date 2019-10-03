-- Module Game

defaultTileSize :: Int
defaultTileSize = 16

defaultMineCount :: Int
defaultMineCount = 100

defaultBoardSize :: (Int, Int)
defaultBoardSize = (32, 16)


-- Restarts a fresh game.
restartGame :: Game -> Game
restartGame g = g { board = board'
                  , gameState = InitialisedGS
                  , minePositions = SM.empty
                  }
    where (boardW, boardH) = boardSize g
          positions = [(x, y) | x <- [0..boardW-1], y <- [0..boardH-1]]
          board' = SM.fromList $ zip positions (repeat $ CoveredTS False)


-- Updates the game in response to inputs.
updateGame :: InputData -> Game -> Game
updateGame input game = game'
    where 
          -- ****************************** Input retrieval ******************************
          mousePosition = mouseBoardPos (mousePos input) game
          mouseLeftPressed = elem (MouseButton LeftButton) (pressedKeys input)
          mouseRightPressed = elem (MouseButton RightButton) (pressedKeys input)
          
          -- ****************************** Game updates ******************************
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


-- Places all the mines after an initial mouse click.
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


-- Reveals the tile at the given position and updates the game consequently.
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

-- Reveals all the bombs.
revealBombs :: Game -> Game
revealBombs game = foldr (\pos g -> revealTile pos g) game $ SM.keys $ minePositions game
         
-- Converts window coordinates to game board coordinates.
mouseBoardPos :: Position -> Game -> (Int, Int)     
mouseBoardPos (Position mX mY) game = let (resW, resH) = viewportRes game
                                          (boardW, boardH) = boardSize game
                                      in (floor $ (fromIntegral mX / fromIntegral resW) * fromIntegral boardW,
                                          floor $ (fromIntegral mY / fromIntegral resH) * fromIntegral boardH)


-- Module Input

newInputData :: InputData
newInputData = InputData [] [] [] (Position 0 0) (Position 0 0)


mouseKeyboardCallback :: IORef InputData -> Key -> KeyState -> Modifiers -> Position -> IO ()
mouseKeyboardCallback inputsIORef key keyState _ newMousePos =
    if keyState == Down
      then inputsIORef $~! \x -> x { onKeys = if elem key (onKeys x)
                                                then onKeys x
                                                else key : (onKeys x)
                                   , pressedKeys = if elem key (onKeys x)
                                                     then pressedKeys x
                                                     else key : (pressedKeys x)
                                   , mousePos = newMousePos }
      else inputsIORef $~! \x -> x { onKeys = L.delete key (onKeys x)
                                   , releasedKeys = key : (releasedKeys x)
                                   , mousePos = newMousePos }



mouseMovementCallback :: IORef InputData -> Position -> IO ()
mouseMovementCallback inputsIORef newMousePos = inputsIORef $~! \x -> x { previousMousePos = (mousePos x)
                                                                        , mousePos = newMousePos }

cancelPressedAndReleasedKeys :: InputData -> InputData
cancelPressedAndReleasedKeys input = input { pressedKeys = [], releasedKeys = [] }
