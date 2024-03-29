-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import System.IO.Unsafe (unsafePerformIO)
import System.IO
import Data.Fixed
import Control.DeepSeq
import Data.IORef

data Counter = Counter { x :: IORef Int }

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step _ (GameOver s) = return $ GameOver s
step _ w@(Pause as ship ufo bs ks l score) = return w
step secs w@(Play as (Ship shipCs shipSpeed@(shipSpeedX, shipSpeedY) shipRot) ufo bs keys l@(currLevel, asCount, uCountDown) s)
  | any (collidesWithAsteroid shipCs) as = return $ loseLife w
  | collidesWithUFO shipCs ufo           = return $ loseLife w
  | otherwise                            = updateLevel secs (inputKeys
                                                  (Play (concatMap updateAsteroid as)
                                                  (Ship updateShipCs updateShipSpeed shipRot)
                                                  (updateUFO ufo)
                                                  (concatMap updateBullet bs)
                                                  keys
                                                  l
                                                  (updateScore s as)))
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
            | age > bulletLifeTime              = [] -- seconds the bullet lives
            | any (collidesWithAsteroid bCs) as = []
            | collidesWithUFO bCs ufo           = []
            | otherwise                         = [Bullet (cycleWorld (bCs .+ (secs .* speed))) speed (age + (secs * 1))]

          updateAsteroid :: Asteroid -> [Asteroid]
          updateAsteroid a@(Asteroid aCs size speed)
            | collidesWithBulletA a && size <= minAsteroidSize = []
            | collidesWithBulletA a && size > minAsteroidSize = split a
            | otherwise                        = [Asteroid (cycleWorld (aCs .+ (secs .* speed))) size speed]

          updateUFO :: UFO -> UFO   -- update UFO coördinates and speed
          updateUFO u@(UFO uCs uSpeed True t)  = case collidesWithBulletU u of
                                                  True -> (UFO (fromIntegral (fst screenRes + 100) , fromIntegral (snd screenRes + 100)) (0, 0) False uCountDown)
                                                  _    -> (UFO (cycleWorld (uCs .+ (secs .* newUSpeed))) newUSpeed True t)
                                                    where newUSpeed = ufoSpeed .* unitVector (shipCs .- uCs)  -- ufo moves towards player
          updateUFO u@(UFO uCs uSpeed False t) = UFO uCs uSpeed False (t - secs)  -- countdown to respawn

          collidesWithAsteroid :: Coordinates -> Asteroid -> Bool
          collidesWithAsteroid cs (Asteroid aCs size speed) = magnetude (aCs .- cs) < size

          collidesWithUFO :: Coordinates -> UFO -> Bool
          collidesWithUFO cs (UFO uCs _ _ _) = magnetude (uCs .- cs) < ufoSize

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

          loseLife :: World -> World
          loseLife (Play as ship ufo bs ks l (lives, score))
            | lives == 1 = GameOver (lives, score)
            | otherwise  = Play as (Ship (0,0)(0,0) 0) ufo bs ks l (lives - 1, score)

          updateScore :: Score -> [Asteroid] -> Score
          updateScore (lives, score) as = (lives, score)

-- | Maps keystroke to corresponding function
inputKeys :: World -> World
inputKeys w@(Play _ _ _ _ ks _ _) = compose (map keyFunc ks) w
    where keyFunc k = case k of
                        Char 'w'            -> thrustForward
                        Char 'a'            -> turnLeft
                        Char 'd'            -> turnRight
                        SpecialKey KeySpace -> shoot
inputKeys w = w

-- | Handle user input
input :: Event -> World -> IO World
input (EventKey key Down _ _) (GameOver s)
  | key == Char 'r' = return initialState
  | key == Char 's' = saveScore $ GameOver s
  | otherwise       = return $ GameOver s
input (EventKey key Down _ _) world@(Play as ship ufo bs ks l s)
  | key == Char 'w'            = return (Play as ship ufo bs (key:ks) l s)
  | key == Char 'a'            = return (Play as ship ufo bs (key:ks) l s)
  | key == Char 'd'            = return (Play as ship ufo bs (key:ks) l s)
  | key == SpecialKey KeySpace = return (Play as ship ufo bs (key:ks) l s)
  | key == Char 'q'            = return (GameOver s)
  | key == Char 'p'            = return (Pause as ship ufo bs ks l s)
  | otherwise                  = return world
input (EventKey key Up _ _) world@(Play as ship ufo bs ks l s)
  | key == Char 'w'            = return (Play as ship ufo bs (delete key ks) l s)
  | key == Char 'a'            = return (Play as ship ufo bs (delete key ks) l s)
  | key == Char 'd'            = return (Play as ship ufo bs (delete key ks) l s)
  | key == Char 'p'            = return (Play as ship ufo bs (delete key ks) l s)
  | key == SpecialKey KeySpace = return (Play as ship ufo bs (delete key ks) l s)
  | otherwise                  = return world
input (EventKey key Down _ _) world@(Pause as ship ufo bs ks l s)
  | key == Char 'p'            = return (Play as ship ufo bs [] l s)  -- return ks = [], so input is reset
  | otherwise                  = return world
input _ world = return world

thrustForward :: World -> World
thrustForward (Play as (Ship shipCs shipSpeed shipRot) ufo bs keys l s) = Play as (Ship shipCs updateShipSpeed shipRot) ufo bs keys l s
    where updateShipSpeed = shipSpeed .+ (applyRotation shipRot thrustPower)

turnRight :: World -> World
turnRight (Play as (Ship cs (x,y) shipRot) ufo bs keys l s) = Play as (Ship cs (x,y) (shipRot + turnSpeed)) ufo bs keys l s

turnLeft :: World -> World
turnLeft (Play as (Ship cs (x,y) shipRot) ufo bs keys l s) = Play as (Ship cs (x,y) (shipRot - turnSpeed)) ufo bs keys l s

shoot :: World -> World
shoot (Play as ship@(Ship shipCs shipSpeed shipRot) ufo bs keys l s) =
  Play  as ship ufo
        ((newBullet $ head' bs) ++ bs)
        keys l s
    where head' :: [Bullet] -> [Bullet]
          head' [] = []
          head' (x:xs) = [x]

          newBullet :: [Bullet] -> [Bullet]
          newBullet [] = [Bullet shipCs (applyRotation shipRot (bulletSpeed .+ shipSpeed)) 0]
          newBullet [(Bullet _ _ age)]
            | age < shootSpeed = []
            | otherwise        = [Bullet shipCs (applyRotation shipRot (bulletSpeed .+ shipSpeed)) 0]

getRandom :: IO Float
getRandom = do  randomNumber <- randomIO
                return randomNumber

saveScore :: World -> IO World
saveScore w@(GameOver (l, score))
  | l == -1   = return w
  | otherwise = do
      handle <- openFile "highScores.txt" ReadWriteMode
      contents <- hGetContents handle
      let newScore = putScore $! contents
      rnf contents `seq` writeFile "highScores.txt" (unlines newScore)
      hClose handle
      return $ GameOver (-1, score)   -- return level = -1, so score won't be saved twice
        where putScore s = reverse $ map show (sort $ (map (\x -> read x :: Int) ((show score):(words s))))

-- | Game logic, levels, spawning asteroids, all that good stuff
updateLevel :: Float -> World -> IO World
updateLevel _ w@(Pause as ship ufo bs keys l s) = return w
updateLevel secs (Play as ship ufo bs keys l@(currLevel, asCount, uCountDown) s)
  | null as   = return $ Play (newAsteroids asCount) ship ufo bs keys (currLevel + 1, min 20 (asCount + 1), max (uCountDown - 1) 5) s
  | otherwise = return $ Play as
                        ship
                        (levelUpdateUFO ufo)
                        bs
                        keys
                        l
                        s
    where levelUpdateUFO :: UFO -> UFO
          levelUpdateUFO u@(UFO uCs uSpeed active t)
            | not active && t <= 0 = UFO ((unsafePerformIO getRandom) * (fromIntegral $ snd screenRes),    -- UFO spawning
                                                if even (length as) then 0 else fromIntegral $ fst screenRes)
                                                (20, 200 + (unsafePerformIO getRandom * 100)) True t
            | otherwise = u                                                                                     -- Return UFO

          newAsteroids :: Int -> [Asteroid] -- Spawn new asteroids
          newAsteroids 0 = []
          newAsteroids n = Asteroid (asteroidSpawnNotColliding ship)
                                    (40 + (fromIntegral $ round (unsafePerformIO getRandom * 10)))
                                    (x, y) : newAsteroids (n-1)
                                       where x = if even $ round (unsafePerformIO getRandom * 10)
                                                then -speed
                                                else speed
                                                  where speed = 20 + unsafePerformIO getRandom * 20
                                             y = if even $ round (unsafePerformIO getRandom * 10)
                                                then -speed
                                                else speed
                                                  where speed = 20 + unsafePerformIO getRandom * 20

          asteroidSpawnNotColliding :: Ship -> (Float, Float)  -- Make sure the asteroids are not spawning on top of the player
          asteroidSpawnNotColliding ship@(Ship shipCs shipSpeed shipRot) = if magnetude (shipCs .- cs) < safeDistance   -- Integer is safe distance around player
                                                                      then asteroidSpawnNotColliding ship
                                                                      else cs
                                                                              where getCoords = if even $ round (unsafePerformIO getRandom * 10)
                                                                                                then (0, unsafePerformIO getRandom * snd screenResF)
                                                                                                else (unsafePerformIO getRandom * fst screenResF, 0)
                                                                                    cs = getCoords