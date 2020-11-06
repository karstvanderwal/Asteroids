-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture
viewPure GameOver = pictures [scale 0.3 0.3 . translate (-400) 0 
                        . color red . text $ "Game Over!",
                        scale 0.2 0.2 . translate (-625) (-550)
                        . color white . text $ "Press r to restart"]
viewPure (Play rocks (Ship (x,y) (vx,vy) rot) (UFO (ux,uy) (uvx, uvy) _ _) bullets _ _)
    = pictures [ship, asteroids, ufo, shots]
    where 
        ship        = color white   (pictures [translate x y (Rotate rot (polygon[(-15,-15),(15,-15),(0,15),(-15,-15)]))])
        asteroids   = pictures      [color orange (polygon (asteroidShape x y s))
                                    | Asteroid   (x,y) s _ <- rocks]
        ufo         = color green   (pictures [translate ux uy (circle 10)])
        shots       = pictures      [translate x y (color red (circle 2))
                                    | Bullet (x,y) _ _ <- bullets]
        
asteroidShape :: Float -> Float -> Float -> [Point]
asteroidShape x y s = [(x,y+s),(x+s,y), (x,y+0.5),(x+0.4*s,y-0.4*s),(x-0.4*s,y+0.5*s),(x-0.2*s,y-0.2*s),(x+0.6*s,y+0.3*s)]