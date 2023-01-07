-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture
viewPure (GameOver (_, score)) = pictures [scale 0.3 0.3 . translate (-400) 100 
                        . color red . text $ "Game Over!",
                        scale 0.2 0.2 . translate (-625) (-250)
                        . color white . text $ "Press r to restart",
                        scale 0.3 0.3 . translate (-200) (-550)
                        . color white . text $ show score,
                        scale 0.2 0.2 . translate (-625) (-450)
                        . color green . text $ "Press s to save highscore"]
viewPure (Play rocks (Ship (x,y) (vx,vy) rot) (UFO (ux,uy) (uvx, uvy) _ _) bullets _ _ (lives, score))
    = pictures [ship, asteroids, ufo, shots, life, scoring]
    where 
        ship      = color white   (pictures [translate x y (Rotate rot (polygon[(-15,-15),(15,-15),(0,15),(-15,-15)]))])
        asteroids = pictures      [color orange (polygon (asteroidShape x y s))
                                  | Asteroid   (x,y) s _ <- rocks]
        ufo       = color green   (pictures [translate ux uy (circle 10)])
        shots     = pictures      [translate x y (color red (circle 2))
                                  | Bullet (x,y) _ _ <- bullets]
        scoring   = pictures [translate (10 - (fst screenResF / 2)) (-30 + (snd screenResF / 2)) . scale 0.2 0.2 . color white . text $ show score]
        life      = pictures $ drawLife lives
        
asteroidShape :: Float -> Float -> Float -> [Point]
asteroidShape x y s = [(x,y+s),(x+s,y), (x,y+0.5),(x+0.4*s,y-0.4*s),(x-0.4*s,y+0.5*s),(x-0.2*s,y-0.2*s),(x+0.6*s,y+0.3*s)] -- [(8, 0), (2, 6), (-6, 4), (-6, -4), (2, -6), (8, 0)]

drawLife :: Float -> [Picture] -- Float must be a whole number
drawLife 0 = []
drawLife n = pictures [translate ((25 * n) - (fst screenResF / 2)) (-50 + (snd screenResF / 2)) . scale 0.7 0.7 . color blue . polygon $ [(-15,-15),(15,-15),(0,15),(-15,-15)]]
              : drawLife (n - 1)