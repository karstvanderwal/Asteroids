-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs (Play as (Ship shipCs shipSpeed) ufo bs) =
    return $ (Play as (Ship updateShipCs updateShipSpeed) ufo bs)

      where updateShipCs :: Coordinates
            updateShipCs = shipCs .+ (secs .* shipSpeed)

            updateShipSpeed :: Speed
            updateShipSpeed = shipSpeed .- (secs .* (0,1))

-- | Handle user input
input :: Event -> World -> IO World
input (EventKey (Char char) Down _ _) world
    | char == 'w' = return (thrustForward world)
    | char == 'a' = return (turnLeft world)
    | char == 'd' = return (turnRight world)
    | char == ' ' = return (shoot world)
    | otherwise = return world
input _ world = return world

--inputKey :: Event -> World -> World
--inputKey (EventKey (Char c) _ _ _) gstate
-- = -- If the user presses a character key, show that one
--    gstate { infoToShow = ShowAChar c }
--inputKey _ gstate = gstate -- otherwise keep the same

thrustForward :: World -> World
thrustForward (Play as (Ship shipCs (x,y)) ufo bs) = Play as (Ship shipCs (x,y+10)) ufo bs

turnRight :: World -> World
turnRight (Play as (Ship cs (x,y)) ufo bs) = undefined

turnLeft :: World -> World
turnLeft (Play as (Ship cs (x,y)) ufo bs) = undefined

shoot :: World -> World
shoot (Play as ship@(Ship shipCs shipSpeed) ufo bs) = Play as ship ufo (newBullet:bs)
    where newBullet = Bullet shipCs (0,30) 0