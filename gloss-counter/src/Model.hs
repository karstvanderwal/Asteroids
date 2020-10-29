-- | This module contains the data types
--   which represent the state of the game
module Model where

initialState :: World
initialState = Play 
                [Asteroid (150,150) 39 (2,6)]
                (Ship (0,-300)(0,0) 0) 
                (UFO (0,200) (2,5)) 
                []

data Ship      = Ship Coordinates Speed Rotation
data World     = Play [Asteroid] Ship UFO [Bullet] | GameOver
data Bullet    = Bullet Coordinates Speed Age
data Asteroid    = Asteroid Coordinates Size Speed
data UFO         = UFO Coordinates Speed

type Coordinates = (Float, Float)

(.-) , (.+) :: (Float,Float) -> (Float,Float) -> (Float,Float)
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> (Float,Float) -> (Float,Float)
s .* (u,v) = (s*u,s*v)

type Rotation    = Float            -- Rotation in degrees
type Speed       = (Float, Float)

magnetude :: (Float,Float) -> Float
magnetude (x, y) = sqrt (x**2 + y**2)

type Age         = Float
type Size        = Float

screenRes :: (Int, Int)
screenRes = (700, 700)