-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Interface.IO.Game
import Prelude
import System.Random

initialState :: World
initialState = Play 
                []
                (Ship (0,0)(0,0) 0)
                (UFO (0,400) (-200,-2) False 10)  -- Spawn UFO after 10 seconds
                []
                []
                (0,6,15)
                (3,0)

data World    = Play [Asteroid] Ship UFO [Bullet] [Key] Level Score 
              | GameOver Score 
              | Pause [Asteroid] Ship UFO [Bullet] [Key] Level Score
data Asteroid = Asteroid Coordinates Size Speed
data Ship     = Ship Coordinates Speed Rotation
data UFO      = UFO Coordinates Speed Bool Time
data Bullet   = Bullet Coordinates Speed Age

type Level = (Int, Int, Float)  -- (level, asteroidCount, respawnTimeUFO)
type Score = (Float, Int)       -- (lives, score), lives must be a whole number

type Coordinates = (Float, Float)
type Speed    = (Float, Float)

(.-) , (.+) :: (Float, Float) -> (Float, Float) -> (Float, Float)
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> (Float, Float) -> (Float, Float)
s .* (u,v) = (s*u,s*v)

magnetude :: (Float, Float) -> Float
magnetude (x, y) = sqrt (x**2 + y**2)

unitVector :: (Float, Float) -> (Float, Float)
unitVector (x, y) = (x / mag, y / mag)
  where mag = magnetude (x, y)

type Rotation = Float            -- Rotation in degrees

applyRotation :: Float -> (Float, Float) -> (Float, Float)          -- First float is in degrees
applyRotation rot (x, y) = (sin degrInRad * x, cos degrInRad * y)
    where degrInRad = rot * 2 * pi / 360

type Age  = Float
type Size = Float

type Time = Float

compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

-- Some variables for easy access
screenRes :: (Int, Int) --screenRes is the size of the window
screenRes = (700, 700)

screenResF :: (Float, Float)
screenResF = (fromIntegral $ fst screenRes, fromIntegral $ snd screenRes)

thrustPower :: Speed -- thrustPower is the amount the speed is increased
thrustPower = (9, 9)

turnSpeed :: Float -- turnSpeed is the speed at which the ship rotates
turnSpeed = 7

bulletSpeed :: Speed -- bulletSpeed is the speed that the bullet gets when shot
bulletSpeed = (450, 450)

bulletLifeTime :: Float -- maximum seconds the bullet is on screen
bulletLifeTime = 1

shootSpeed :: Float -- Shoot a bullet every shootSpeed seconds
shootSpeed = 0.2

ufoSize :: Float -- size of the ufo, hitbox and draw
ufoSize = 20

ufoSpeed :: Float -- speed of the ufo
ufoSpeed = 80

minAsteroidSize :: Float -- asteroid disappears when it's very small
minAsteroidSize = 19

safeDistance :: Float -- asteroids won't spawn in this radius
safeDistance = 80