-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step _ GameOver = return GameOver
step secs (Play as (Ship shipCs shipSpeed@(shipSpeedX, shipSpeedY) rot) ufo bs)
    | any (collidesWithAsteroid shipCs) as = return GameOver
    | collidesWithUFO shipCs ufo           = return GameOver
    | otherwise                            = return (Play (concatMap updateAsteroid as) 
                                                    (Ship updateShipCs updateShipSpeed rot) 
                                                    ufo 
                                                    (concatMap updateBullet bs))
      where updateShipCs :: Coordinates
            updateShipCs = shipCs .+ (secs .* shipSpeed)

            updateShipSpeed :: Speed
            updateShipSpeed
                | shipSpeedX < minimalSpeed && shipSpeedY < minimalSpeed = shipSpeed .- (secs .* (shipSpeedX,shipSpeedY))
                | otherwise                                              = shipSpeed .- (secs .* (shipSpeedX / speedRed, shipSpeedY / speedRed))
                  where minimalSpeed   = 8 --to make ship stand still eventually
                        speedRed = 3 --speed/this number is the amount it's lowered every second

            updateBullet :: Bullet -> [Bullet]
            updateBullet (Bullet bCs speed age)
                | age > 3                           = [] --seconds the bullet lives
                | any (collidesWithAsteroid bCs) as = []
                | otherwise                         = [Bullet (bCs .+ (secs .* speed)) speed (age + (secs * 1))]

            updateAsteroid :: Asteroid -> [Asteroid]
            updateAsteroid a@(Asteroid aCs size speed)
                | collidesWithBullet a && size < 7 = []
                | collidesWithBullet a && size > 7 = split a
                | otherwise                        = [Asteroid (aCs .+ (secs .* speed)) size speed]

            collidesWithAsteroid :: Coordinates -> Asteroid -> Bool
            collidesWithAsteroid cs (Asteroid aCs size speed) = magnetude (aCs .- cs) < size

            collidesWithUFO :: Coordinates -> UFO -> Bool
            collidesWithUFO cs (UFO uCs _) = magnetude (uCs .- cs) < 10 --UFO size

            collidesWithBullet :: Asteroid -> Bool
            collidesWithBullet a = any (\(Bullet cs _ _) -> collidesWithAsteroid cs a) bs

            split :: Asteroid -> [Asteroid]
            split (Asteroid aCs size speed) = [Asteroid aCs (size/2) (2 .* (applyRotation (pi/3) speed)),
                                               Asteroid aCs (size/2) (2 .* (applyRotation (-pi/3) speed))]

-- | Handle user input
input :: Event -> World -> IO World
input (EventKey (Char char) Down _ _) GameOver
    | char == 'r' = return initialState
    | otherwise   = return GameOver
input (EventKey (Char char) Down _ _) world
    | char == 'w' = return (thrustForward world)
    | char == 'a' = return (turnLeft world)
    | char == 'd' = return (turnRight world)
    | char == ' ' = return (shoot world)
    | otherwise = return world
input _ world = return world

thrustForward :: World -> World
thrustForward (Play as (Ship shipCs shipSpeed rot) ufo bs) = Play as (Ship shipCs updateShipSpeed rot) ufo bs
    where updateShipSpeed = shipSpeed .+ (applyRotation rot (20,20))

turnRight :: World -> World
turnRight (Play as (Ship cs (x,y) rot) ufo bs) = Play as (Ship cs (x,y) (rot + 30)) ufo bs

turnLeft :: World -> World
turnLeft (Play as (Ship cs (x,y) rot) ufo bs) = Play as (Ship cs (x,y) (rot - 30)) ufo bs

shoot :: World -> World
shoot (Play as ship@(Ship shipCs _ rot) ufo bs) = Play as ship ufo (newBullet:bs)
    where newBullet = Bullet shipCs (applyRotation rot (50,50)) 0

applyRotation :: Float -> (Float, Float) -> (Float, Float)
applyRotation rot (x, y) = (sin degrInRad * x, cos degrInRad * y)
    where degrInRad = rot * 2 * pi / 360