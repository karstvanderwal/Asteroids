-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0

data Ship      = Ship Coordinates Speed
data World     = Play [Asteroid] Ship UFO [Bullet] | GameOver
data Bullet    = Bullet Coordinates Speed Age
data Asteroid    = Asteroid Coordinates Size Speed
data UFO         = UFO Coordinates Speed

type Coordinates = (Float, Float)
type Speed       = (Float, Float)
type Age         = Float
type Size        = Float