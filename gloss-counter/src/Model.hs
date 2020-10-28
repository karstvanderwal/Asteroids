-- | This module contains the data types
--   which represent the state of the game
module Model where

initialState :: World
initialState = Play 
                [Asteroid (150,150) 20 (2,6)]
                (Ship (0,-300)(0,0)) 
                (UFO (0,200) (2,5)) 
                []

data Ship      = Ship Coordinates Speed
data World     = Play [Asteroid] Ship UFO [Bullet] | GameOver
data Bullet    = Bullet Coordinates Speed Age
data Asteroid    = Asteroid Coordinates Size Speed
data UFO         = UFO Coordinates Speed

type Coordinates = (Float, Float)

(.-) , (.+) :: Coordinates -> Coordinates -> Coordinates
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> Coordinates -> Coordinates
s .* (u,v) = (s*u,s*v)

type Speed       = (Float, Float)
type Age         = Float
type Size        = Float