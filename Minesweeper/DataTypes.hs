-- Module Game

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

-- Module Input

data InputData = InputData { onKeys :: ![Key]              -- Keys that are currently down.
                           , pressedKeys :: ![Key]         -- Keys that were pressed during this iteration.
                           , releasedKeys :: ![Key]        -- Keys that were released during this iteration.
                           , mousePos :: !Position         -- The current mouse position.
                           , previousMousePos :: !Position -- The last recorded mouse position before the current mouse position. Useful for detecting the distance of mouse movements.
                           }


