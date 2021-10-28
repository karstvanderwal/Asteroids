-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Interface.IO.Game

initialState :: World
initialState = Play 
                [Asteroid (150,150) 60 (-30,22),Asteroid (200,0) 45 (39,18),
                Asteroid (-200,-40) 52 (-20,-31),Asteroid (-100,-350) 57 (25,-19)]
                (Ship (0,0)(0,0) 0)
                (UFO (0,200) (-200,-2) True 15)
                []
                []
                (0,6,15)
                (3,0)

data World    = Play [Asteroid] Ship UFO [Bullet] [Key] Level Score | GameOver Score
data Asteroid = Asteroid Coordinates Size Speed
data Ship     = Ship Coordinates Speed Rotation
data UFO      = UFO Coordinates Speed Bool Time
data Bullet   = Bullet Coordinates Speed Age

type Coordinates = (Float, Float)
type Speed    = (Float, Float)

(.-) , (.+) :: (Float,Float) -> (Float,Float) -> (Float,Float)
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> (Float,Float) -> (Float,Float)
s .* (u,v) = (s*u,s*v)

magnetude :: (Float,Float) -> Float
magnetude (x, y) = sqrt (x**2 + y**2)

type Rotation = Float            -- Rotation in degrees

applyRotation :: Float -> (Float, Float) -> (Float, Float)          -- First float is in degrees
applyRotation rot (x, y) = (sin degrInRad * x, cos degrInRad * y)
    where degrInRad = rot * 2 * pi / 360

type Age  = Float
type Size = Float

type Time = Float

compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

type Level = (Int, Int, Float)  -- (level, asteroidCount, respawnTimeUFO)
type Score = (Float, Int)       -- (lives, score), lives must be a whole number

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