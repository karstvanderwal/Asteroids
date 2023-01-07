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

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step _ (GameOver s) = return $ GameOver s
step secs w@(Play as (Ship shipCs shipSpeed@(shipSpeedX, shipSpeedY) rot) ufo bs keys l@(currLevel, asCount, uCountDown) s)
  | any (collidesWithAsteroid shipCs) as = return $ loseLife w
  | collidesWithUFO shipCs ufo           = return $ loseLife w
  | otherwise                            = updateLevel secs (inputKeys 
                                                  (Play (concatMap updateAsteroid as)
                                                        (Ship updateShipCs updateShipSpeed rot)
                                                        (updateUFO ufo)
                                                        (concatMap updateBullet bs)
                                                        keys
                                                        l
                                                        (updateScore s)))
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
            | collidesWithBulletA a && size <= 19 = []
            | collidesWithBulletA a && size > 19 = split a
            | otherwise                        = [Asteroid (cycleWorld (aCs .+ (secs .* speed))) size speed]

          updateUFO :: UFO -> UFO
          updateUFO u@(UFO uCs uSpeed True t)  = case collidesWithBulletU u of 
                                                  True -> (UFO (fromIntegral (fst screenRes + 100) , fromIntegral (snd screenRes + 100)) (0, 0) False uCountDown)
                                                  _    -> (UFO (cycleWorld (uCs .+ (secs .* uSpeed))) uSpeed True t)
          updateUFO u@(UFO uCs uSpeed False t) = (UFO uCs uSpeed False (t - secs))

          collidesWithAsteroid :: Coordinates -> Asteroid -> Bool
          collidesWithAsteroid cs (Asteroid aCs size speed) = magnetude (aCs .- cs) < (size / 2)

          collidesWithUFO :: Coordinates -> UFO -> Bool
          collidesWithUFO cs (UFO uCs _ _ _) = magnetude (uCs .- cs) < 20 --UFO size

          collidesWithBulletA :: Asteroid -> Bool
          collidesWithBulletA a = any (\(Bullet cs _ _) -> collidesWithAsteroid cs a) bs

          collidesWithBulletU :: UFO -> Bool
          collidesWithBulletU u = any (\(Bullet cs _ _) -> collidesWithUFO cs u) bs

          split :: Asteroid -> [Asteroid]
          split (Asteroid aCs size speed) = [Asteroid aCs (size/2) (2 .* (applyRotation (unsafePerformIO getRandomF * 180)) speed),
                                            Asteroid aCs (size/2) (2 .* (applyRotation (unsafePerformIO getRandomF * (-180)) speed))]

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
          inputKeys w@(Play _ _ _ _ ks _ _) = compose (map keyFunc ks) w
              where keyFunc k = case k of
                                  Char 'w'            -> thrustForward
                                  Char 'a'            -> turnLeft
                                  Char 'd'            -> turnRight
                                  SpecialKey KeySpace -> shoot
          inputKeys (GameOver s) = GameOver s

          loseLife :: World -> World
          loseLife (Play as (Ship shipCs shipSpeed@(shipSpeedX, shipSpeedY) rot) ufo bs ks l (lives, score)) 
            | lives == 1 = GameOver (lives, score)
            | otherwise  = Play as (Ship (0,0)(0,0) 0) ufo bs ks l (lives - 1, score)

          updateScore :: Score -> Score
          updateScore (x,y) = (x,y)



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
  | key == Char 'q'            = return (GameOver (s))
  | otherwise                  = return world
input (EventKey key Up _ _) world@(Play as ship ufo bs ks l s)
  | key == Char 'w'            = return (Play as ship ufo bs (delete key ks) l s)
  | key == Char 'a'            = return (Play as ship ufo bs (delete key ks) l s)
  | key == Char 'd'            = return (Play as ship ufo bs (delete key ks) l s)
  | key == SpecialKey KeySpace = return (Play as ship ufo bs (delete key ks) l s)
  | otherwise                  = return world
input _ world = return world

thrustForward :: World -> World
thrustForward (Play as (Ship shipCs shipSpeed rot) ufo bs keys l s) = Play as (Ship shipCs updateShipSpeed rot) ufo bs keys l s
    where updateShipSpeed = shipSpeed .+ (applyRotation rot thrustPower)

turnRight :: World -> World
turnRight (Play as (Ship cs (x,y) rot) ufo bs keys l s) = Play as (Ship cs (x,y) (rot + turnSpeed)) ufo bs keys l s

turnLeft :: World -> World
turnLeft (Play as (Ship cs (x,y) rot) ufo bs keys l s) = Play as (Ship cs (x,y) (rot - turnSpeed)) ufo bs keys l s

shoot :: World -> World
shoot (Play as ship@(Ship shipCs shipSpeed rot) ufo bs keys l s) = Play as ship ufo ((newBullet $ head' bs) ++ bs) keys l s
    where head' :: [Bullet] -> [Bullet]
          head' [] = []
          head' (x:xs) = [x]

          newBullet :: [Bullet] -> [Bullet]
          newBullet [] = [Bullet shipCs (applyRotation rot (bulletSpeed .+ shipSpeed)) 0]
          newBullet [(Bullet _ _ age)]
            | age < shootSpeed = []            
            | otherwise        = [Bullet shipCs (applyRotation rot (bulletSpeed .+ shipSpeed)) 0]

{- getRandomF :: Float
getRandomF = 
  let (value, newGen) = random getStdGen :: (Float, StdGen)
  in  setStdGen' newGen
  

getRandomI :: (Int, Int) -> Int
getRandomI (a, b) = 
  let (value, newGen) = randomR (a, b) getStdGen :: (Int, StdGen)
  in  value 
  setStdGen' newGen

getRandomB :: Bool
getRandomB = 
  let (value, newGen) = random getStdGen :: (Bool, StdGen)
    in setStdGen' newGen -}

setStdGen' :: StdGen -> ()
setStdGen' g = unsafePerformIO $ do setStdGen g

saveScore :: World -> IO World
saveScore w@(GameOver (l, score))
  | l == -1   = return w
  | otherwise = do
      return $ GameOver (-1, score)



-- Game logic, levels, spawning asteroids, all that good stuff
updateLevel :: Float -> World -> IO World
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
            | active == False && t <= 0 = UFO (fromIntegral $ getRandomI (-fst screenRes, fst screenRes), if even (length as) then 0 else (fromIntegral $ fst screenRes))
                                              (200 + (getRandomF * 100), 20) 
                                              True 
                                              t
            | otherwise = u

          newAsteroids :: Int -> [Asteroid]
          newAsteroids 0 = []
          newAsteroids n = (Asteroid (if getRandomB then (0, getRandomF * snd screenResF) else (getRandomF * fst screenResF, 0))
                                      (40 + (fromIntegral getRandomI (0, 10)))
                                      (20 + getRandomF * 30, 20 + getRandomF * 30)) : newAsteroids (n-1)