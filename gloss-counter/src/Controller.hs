-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import System.IO.Unsafe (unsafePerformIO)
import Data.Fixed

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step _ GameOver = return GameOver
step secs (Play as (Ship shipCs shipSpeed@(shipSpeedX, shipSpeedY) rot) ufo bs keys l@(currLevel, asCount, uCountDown))
  | any (collidesWithAsteroid shipCs) as = return GameOver
  | collidesWithUFO shipCs ufo           = return GameOver
  | otherwise                            = updateLevel secs (inputKeys 
                                                  (Play (concatMap updateAsteroid as) 
                                                  (Ship updateShipCs updateShipSpeed rot) 
                                                  (updateUFO ufo)
                                                  (concatMap updateBullet bs)
                                                  keys
                                                  l))
    where updateShipCs :: Coordinates
          updateShipCs = cycleWorld (shipCs .+ (secs .* shipSpeed))

          updateShipSpeed :: Speed
          updateShipSpeed
            | magnetude shipSpeed < minimalSpeed = (0,0)
            | otherwise                          = shipSpeed .- (secs .* (shipSpeedX / speedRed, shipSpeedY / speedRed))
              where minimalSpeed = 6 --to make ship stand still eventually
                    speedRed     = 3 --speed/this number is the amount it's lowered every second

          updateBullet :: Bullet -> [Bullet]
          updateBullet (Bullet bCs speed age)
            | age > 1.2                         = [] -- seconds the bullet lives
            | any (collidesWithAsteroid bCs) as = []
            | collidesWithUFO bCs ufo           = []
            | otherwise                         = [Bullet (cycleWorld (bCs .+ (secs .* speed))) speed (age + (secs * 1))]

          updateAsteroid :: Asteroid -> [Asteroid]
          updateAsteroid a@(Asteroid aCs size speed)
            | collidesWithBulletA a && size <= 17 = []
            | collidesWithBulletA a && size > 17 = split a
            | otherwise                        = [Asteroid (cycleWorld (aCs .+ (secs .* speed))) size speed]

          updateUFO :: UFO -> UFO
          updateUFO u@(UFO uCs uSpeed True t)  = case collidesWithBulletU u of 
                                                  True -> (UFO (fromIntegral (fst screenRes + 100) , fromIntegral (snd screenRes + 100)) (0, 0) False uCountDown)
                                                  _    -> (UFO (cycleWorld (uCs .+ (secs .* uSpeed))) uSpeed True t)
          updateUFO u@(UFO uCs uSpeed False t) = (UFO uCs uSpeed False (t - secs))

          collidesWithAsteroid :: Coordinates -> Asteroid -> Bool
          collidesWithAsteroid cs (Asteroid aCs size speed) = magnetude (aCs .- cs) < size

          collidesWithUFO :: Coordinates -> UFO -> Bool
          collidesWithUFO cs (UFO uCs _ _ _) = magnetude (uCs .- cs) < 10 --UFO size

          collidesWithBulletA :: Asteroid -> Bool
          collidesWithBulletA a = any (\(Bullet cs _ _) -> collidesWithAsteroid cs a) bs

          collidesWithBulletU :: UFO -> Bool
          collidesWithBulletU u = any (\(Bullet cs _ _) -> collidesWithUFO cs u) bs

          split :: Asteroid -> [Asteroid]
          split (Asteroid aCs size speed) = [Asteroid aCs (size/2) (2 .* (applyRotation (unsafePerformIO getRandom * 180)) speed),
                                            Asteroid aCs (size/2) (2 .* (applyRotation (unsafePerformIO getRandom * (-180)) speed))]

          cycleWorld :: Coordinates -> Coordinates
          cycleWorld (x, y)
            | x < -screenX = (screenX, y)
            | y < -screenY = (x, screenY)
            | x > screenX  = (-screenX, y)
            | y > screenY  = (x, -screenY)
            | otherwise    = (x, y)
              where screenX :: Float
                    screenX = fromIntegral (fst screenRes `div` 2) :: Float
                    screenY = fromIntegral (snd screenRes `div` 2) :: Float
          
          inputKeys :: World -> World
          inputKeys w@(Play _ _ _ _ ks _) = compose (map keyFunc ks) w
              where keyFunc k = case k of
                                  Char 'w'            -> thrustForward
                                  Char 'a'            -> turnLeft
                                  Char 'd'            -> turnRight
          inputKeys GameOver = GameOver

-- | Handle user input
input :: Event -> World -> IO World
input (EventKey key Down _ _) GameOver
  | key == Char 'r' = return initialState
  | otherwise       = return GameOver
input (EventKey key Down _ _) world@(Play as ship ufo bs ks l)
  | key == Char 'w'            = return (Play as ship ufo bs (key:ks) l)
  | key == Char 'a'            = return (Play as ship ufo bs (key:ks) l)
  | key == Char 'd'            = return (Play as ship ufo bs (key:ks) l)
  | key == SpecialKey KeySpace = return $ shoot world
  | otherwise                  = return world
input (EventKey key Up _ _) world@(Play as ship ufo bs ks l)
  | key == Char 'w'            = return (Play as ship ufo bs (delete key ks) l)
  | key == Char 'a'            = return (Play as ship ufo bs (delete key ks) l)
  | key == Char 'd'            = return (Play as ship ufo bs (delete key ks) l)
  | otherwise                  = return world
input _ world = return world

thrustForward :: World -> World
thrustForward (Play as (Ship shipCs shipSpeed rot) ufo bs keys l) = Play as (Ship shipCs updateShipSpeed rot) ufo bs keys l
    where updateShipSpeed = shipSpeed .+ (applyRotation rot thrustPower)

turnRight :: World -> World
turnRight (Play as (Ship cs (x,y) rot) ufo bs keys l) = Play as (Ship cs (x,y) (rot + turnSpeed)) ufo bs keys l

turnLeft :: World -> World
turnLeft (Play as (Ship cs (x,y) rot) ufo bs keys l) = Play as (Ship cs (x,y) (rot - turnSpeed)) ufo bs keys l

shoot :: World -> World
shoot (Play as ship@(Ship shipCs shipSpeed rot) ufo bs keys l) = Play as ship ufo (newBullet:bs) keys l
    where newBullet = Bullet shipCs (applyRotation rot (bulletSpeed .+ shipSpeed)) 0

getRandom :: IO Float 
getRandom = do  randomNumber <- randomIO
                print randomNumber
                return randomNumber



-- Game logic, levels, spawning asteroids, all that good stuff
updateLevel :: Float -> World -> IO World
updateLevel secs (Play as ship ufo bs keys l@(currLevel, asCount, uCountDown))
  | null as   = return $ Play (newAsteroids asCount) ship ufo bs keys (currLevel + 1, min 20 (asCount + 1), max (uCountDown - 1) 5)
  | otherwise = return $ Play as
                        ship
                        (levelUpdateUFO ufo)
                        bs
                        keys
                        l
    where levelUpdateUFO :: UFO -> UFO
          levelUpdateUFO u@(UFO uCs uSpeed active t)
            | active == False && t <= 0 = UFO ((unsafePerformIO getRandom) * (fromIntegral $ snd screenRes), 
                                                if even (length as) then 0 else (fromIntegral $ fst screenRes)) 
                                                (20, 200 + (unsafePerformIO getRandom * 100)) True t
            | otherwise = u

          newAsteroids :: Int -> [Asteroid]
          newAsteroids 0 = []
          newAsteroids n = (Asteroid (case (even $ round (unsafePerformIO getRandom * 10)) of
                                        True  -> (0, unsafePerformIO getRandom * snd screenResF)
                                        False -> (unsafePerformIO getRandom * fst screenResF, 0))
                            (40 + (fromIntegral $ round (unsafePerformIO getRandom * 10)))
                            (0,0)) : newAsteroids (n-1)