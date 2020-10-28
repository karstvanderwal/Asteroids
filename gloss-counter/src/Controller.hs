-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs gstate =
    return $ gstate

-- | Handle user input
input :: Event -> World -> IO World
input e gstate = return gstate

--inputKey :: Event -> World -> World
--inputKey (EventKey (Char c) _ _ _) gstate
-- = -- If the user presses a character key, show that one
--    gstate { infoToShow = ShowAChar c }
--inputKey _ gstate = gstate -- Otherwise keep the same