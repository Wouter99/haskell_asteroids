-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.List
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width :: Int
width = 1200

height :: Int
height = 800

--nO_SECS_BETWEEN_CYCLES :: Float
--nO_SECS_BETWEEN_CYCLES = 5

degRad :: Float
degRad = 0.0174532925

radDeg :: Float
radDeg = 57.2957795

data GameState = GameState{
                           asteroids :: [Asteroid], 
                           ship :: Ship, 
                           bullets :: [Bullet],
                           enemies :: [Enemy],
                           elapsedTime :: Float,
                           pressedKeys :: [Key],
                           onScreen :: (Score, Pause, GameOver)
                          } 

-- Auxiliary datatypes

type Position = (Float, Float) --representeert het middelpunt van het object

type Speed = Float 

type Size = Float  -- 10, 20 of 30

type Friendly = Bool

type Direction = Float --measured in degrees. can be converted to radians with degRad. 

data Version = Shoot | Follow

type Lives = Int

type Score = Int

type Pause = Bool

type GameOver = Bool

-- initalState willen we random, maar dan moet het een IO GameState worden. Is dat een probleem?

-- Main datatypes
data Ship = Ship Position Speed Direction Size Lives Explosion

data Asteroid = Asteroid Position Direction Size  --sizes are 15, 35 or 60. 

data Enemy = Enemy Position Direction Version

data Bullet = Bullet Position Direction Friendly

data HitBox = HitBox Position Shape

data Shape = Rect Float Float | Circ Float

data Explosion = Explosion Position Float | None --Float is time of creation

class Destroyable a where
  destroy :: a -> [a]

instance Destroyable Asteroid where
  destroy = destroyAs

instance Destroyable Enemy where
  destroy _ = []

class Movable a where
  move :: a -> a

class Collidable a where
  hitBox :: a -> HitBox

instance Collidable Ship where
  hitBox (Ship pos sp dir sz lives ex) = HitBox pos (Rect sz (sz/2))
 
instance Collidable Asteroid where
  hitBox (Asteroid p _ sz) = HitBox p (Circ sz)

instance Collidable Enemy where 
  hitBox (Enemy p _ Shoot) = HitBox p (Circ 20)   --later size aanpassen
  hitBox (Enemy p _ Follow) = HitBox p (Circ 30)
  
instance Collidable Bullet where
  hitBox (Bullet p _ _) = HitBox p (Circ 5) -- later aanpassen size


instance Movable Ship where
  move (Ship pos sp dir sz lives ex) = Ship (wrap(newPos sp pos dir)) sp dir sz lives ex  

instance Movable Asteroid where
  move (Asteroid pos dir sz) = Asteroid (wrap(newPos (sizeToSpeed sz) pos dir)) dir sz 

instance Movable Bullet where
  move (Bullet pos dir fr) = Bullet (newPos 20 pos dir) dir fr   

------------------
--helper functions for move implementation
------------------

wrap :: Position -> Position   --uses trans and transBack to translate to a coordinatie system with the origin in the bottom left corner instead of in the middle, wrap the position like a torus, and translate back.
wrap = transBack . wrap' . trans 
  where
    wrap' (x,y) = (fromIntegral((round(x-1) `mod` width) + 1), fromIntegral((round(y-1) `mod` height) + 1))
    trans (x,y) = (x+ (fromIntegral(width)/2) , y + (fromIntegral(height)/2))
    transBack (x,y) = (x- (fromIntegral(width)/2) , y - (fromIntegral(height)/2))

newPos :: Speed -> Position -> Direction -> Position
newPos sp (x,y) dir = (x+sp*cos(degRad*dir), y+sp*sin(degRad*dir))

sizeToSpeed :: Size -> Speed
sizeToSpeed 60 = 1
sizeToSpeed 35 = 2.5
sizeToSpeed 15 = 4 --later functie vinden die beter werkt

moveBack :: Ship -> Ship
moveBack (Ship (x,y) sp dir sz lives ex) = Ship (wrap((x-sp*cos(degRad*dir), y-sp*sin(degRad*dir)))) sp dir sz lives ex

moveEnemy :: Ship -> Enemy -> Enemy  --Aparte move functie voor de enemy omdat deze het extra argument schip nodig heeft om de nieuwe richting te bepalen
moveEnemy _ (Enemy (x1,y1) dir Shoot)= Enemy (wrap(newPos 3 (x1,y1) dirRand)) dirRand Shoot --speed aanpassen
  where
    dirRand = dir+ 0.01*(fromIntegral((round(x1 + y1)) `rem` 51))
moveEnemy (Ship (a,b) _ _ _ _ _) (Enemy (x2,y2) dir Follow) = Enemy (wrap(newPos 4 (x2,y2) dirPlayer)) dirPlayer Follow --speed aanpassen
  where 
    dirPlayer = ((atan2 (y2-b) (x2-a))*radDeg)+180 --atan2 calculates the angle between vector and x -axis, this is translated to degree in range 0 to 360. 


-----------------------
--helper functions for handling user input in the controller
-----------------------

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

isElement :: Eq a => a -> [a] -> Bool
isElement a [] = False
isElement a (x:xs) | a == x = True   
                   | otherwise = isElement a xs

rotateShip :: Ship -> Direction -> Ship
rotateShip (Ship pos sp dir sz lives ex) r = Ship pos sp (dir + r) sz lives ex

playerShoot :: Ship -> [Bullet] -> [Bullet]
playerShoot (Ship pos sp dir sz lives ex) bs = (Bullet pos dir True):bs

enemyShoot :: GameState -> Enemy -> [Bullet]
enemyShoot gstate (Enemy (x,y) dir Shoot) | ((round(time*1000) `mod` 3000) == 0) = [Bullet (x,y) dirPlayer False]   --Enemies van type Shoot schieten elke 3 sec voor een duizendste van een seconde, wat neerkomt op 1 bullet. 
                                          | otherwise = []                                  
  where 
    time = (elapsedTime gstate)
    (Ship (a,b) _ _ _ _ _) = (ship gstate)
    dirPlayer = ((atan2 (y-b) (x-a))*radDeg)+180 --atan2 calculates the angle between vector and x -axis, this is translated to degree in range 0 to 360. 
enemyShoot _ _ = []    --alle andere enemies schieten niet
---------------------- 
--helper functions for building a random GameState
----------------------

mkAsteroid :: (Float,Float,Float) -> Asteroid
mkAsteroid (x,y,dir) = Asteroid (x,y) dir 60

mkEnemy :: (Float,Float,Float,Int) -> Enemy --more versions can be added
mkEnemy (x,y,dir,1) = Enemy (x,y) dir Follow
mkEnemy (x,y,dir,2) = Enemy (x,y) dir Shoot

buildInitial :: [Asteroid] -> [Enemy] -> GameState
buildInitial as es = GameState {asteroids = as, ship = Ship (0,0) 8 90 50 3 None, bullets = [], enemies = es, elapsedTime = 0, pressedKeys = [], onScreen = (0,False, False)}

mkAsteroids :: Int -> IO [Asteroid]
mkAsteroids 0 = return []
mkAsteroids n = do posAs <- createRandomPos n     
                   let (xAs,yAs) = unzip posAs
                   randDirsAs <- createRandomDir n
                   return (map mkAsteroid (zip3 xAs yAs randDirsAs))

{-                 randIntsAs <- createRandomInts n
                   let randSizesAs = map (fromIntegral.(10*)) randIntsAs         
                   createRandomInts :: Int -> IO [Int]
                   createRandomInts n = (sequence $ replicate n $ randomRIO(1,3)) -}

mkEnemies :: Int -> IO [Enemy]
mkEnemies 0 = return []
mkEnemies n = do posEn <-createRandomPos n 
                 let (xEn, yEn) = unzip posEn
                 randDirsEn <- createRandomDir n
                 randVersEn <- sequence $ replicate n $ randomRIO(1,2) 
                 return (map mkEnemy (zip4 xEn yEn randDirsEn randVersEn))

createRandomPos :: Int ->  IO [Position]
createRandomPos n = sequence $ replicate n $ randomPos
                         
randomPos :: IO Position
randomPos = do edge <- randomRIO(1,4)
               posOnEdge edge

posOnEdge :: Int -> IO Position -- 1 is noord, 2 is oost, 3 is zuid, 4 is west
posOnEdge 1 = do randx <- randomRIO((-fromIntegral(width))/2, (fromIntegral(width))/2) :: IO Float
                 return (randx,0.5*(fromIntegral(height)))
posOnEdge 2 = do randy <- randomRIO((-fromIntegral(height))/2, (fromIntegral(height))/2) :: IO Float
                 return (0.5*fromIntegral(width), randy)
posOnEdge 3 = do randx <- randomRIO((-fromIntegral(width))/2, (fromIntegral(width))/2) :: IO Float
                 return (randx,-0.5*(fromIntegral(height)))
posOnEdge 4 = do randy <- randomRIO((-fromIntegral(height))/2, (fromIntegral(height))/2) :: IO Float
                 return (-0.5*fromIntegral(width), randy)
posOnEdge _ = error "not an edge"

{-
    where createRandomPos n = do randxs <- sequence $ replicate n $ randomRIO((-width) `div` 2 , width `div` 2)
                                 randys <- sequence $ replicate n $ randomRIO((-height) `div` 2, height `div` 2)
                                 return (zip (map fromIntegral randxs) (map fromIntegral randys))
-}

createRandomDir :: Int -> IO [Float]
createRandomDir n = (sequence $ replicate n $ randomRIO(0, 360))


--------------------------------
--Functions to update the gamestate
--------------------------------
update :: GameState -> GameState
update gstate = handleCollisions $ moveAll $ shootAll gstate

shootAll :: GameState -> GameState
shootAll gstate = gstate {bullets = ((bullets gstate) ++ concatMap f (enemies gstate))}
  where f :: Enemy -> [Bullet]
        f = enemyShoot gstate

moveAll :: GameState -> GameState
moveAll gstate = gstate {bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map (moveEnemy (ship gstate)) (enemies gstate)}
    
handleCollisions :: GameState -> GameState 
handleCollisions (GameState as ship bs es time keys (sc,pause,gameover)) = GameState as'' ship''' bs' es'' time keys (sc',pause,gameover)   
  where      
    (as', ship', bs', es', sc') = bulletCheckers as ship bs es sc time   --time is needed for keeping track of the time an explosion is initiated
    (as'', ship'') = asteroidChecker as' ship' time
    (es'', ship''') = enemyChecker es' ship'' time     

bulletCheckers :: [Asteroid] -> Ship -> [Bullet] -> [Enemy] -> Score -> Float -> ([Asteroid], Ship, [Bullet], [Enemy], Score)
bulletCheckers as ship bs es sc time = (as', ship', bs''', es',sc'')
  where (as', bs',sc') = bulletChecker as bs sc
        (es', bs'',sc'') = bulletChecker es bs' sc'
        (ship', bs''') = bulletShipChecker ship bs'' time

bulletChecker :: (Destroyable a, Collidable a) => [a] -> [Bullet] -> Score -> ([a], [Bullet], Score)   --For all the enemies and asteroids it checks if they collide with any of the bullets
bulletChecker [] bs sc = ([], bs, sc)
bulletChecker (x:xs) bs sc= ((xs'++xs''), bs'',sc'')
  where (xs', bs', sc') = bulletChecker' x bs sc            --check if first destroyable collides with any of the bullets.
        (xs'', bs'',sc'') = bulletChecker xs bs' sc'        -- recursively check the rest.

bulletChecker' :: (Destroyable a, Collidable a) => a -> [Bullet] -> Score -> ([a], [Bullet], Score)  --checks if bullets collide with asteroids or enemies
--Als bullets leeg zijn dan klaar
bulletChecker' x [] sc= ([x], [], sc)  
bulletChecker' x (b@(Bullet _ _ True):bs) sc | collision x b = (destroy(x), bs,sc+1)
                                             | otherwise = let (xs', bs',sc') = (bulletChecker' x bs sc) in (xs', (b:bs'), sc')
bulletChecker' x (b@(Bullet _ _ False):bs) sc = let (xs', bs', sc') = (bulletChecker' x bs sc) in (xs', (b:bs'), sc')

bulletShipChecker :: Ship -> [Bullet] -> Float -> (Ship, [Bullet])
bulletShipChecker ship [] _ = (ship, [])
bulletShipChecker ship (b@(Bullet _ _ False):bs) time | collision ship b = ((shipDeath time ship), bs) 
                                                      | otherwise = let (ship', bs') = (bulletShipChecker ship bs time) in (ship', b:bs')
bulletShipChecker ship (b@(Bullet _ _ True):bs) time = let (ship', bs') = (bulletShipChecker ship bs time) in (ship', b:bs')


asteroidChecker :: [Asteroid] -> Ship -> Float-> ([Asteroid], Ship)
asteroidChecker [] ship _ = ([], ship)
asteroidChecker (a:as) ship time | collision ship a = ((destroyAs(a)++as), (shipDeath time ship)) 
                                 | otherwise = let (as', ship') = (asteroidChecker as ship time) in ((a:as'),ship')                      

enemyChecker :: [Enemy] -> Ship -> Float -> ([Enemy], Ship)
enemyChecker [] ship _ = ([], ship)
enemyChecker (e:es) ship time | collision ship e = (es, (shipDeath time ship))   
                              | otherwise = let (es', ship') = (enemyChecker es ship time) in ((e:es'), ship')                

shipDeath :: Float -> Ship -> Ship
shipDeath _ (Ship pos sp dir sz 0 ex) = Ship (10000,0) sp dir sz 0 None  --game over, ship just needs to be invisible 
shipDeath time (Ship pos sp dir sz lv ex) = Ship (0,0) sp dir sz (lv-1) (Explosion pos time) 

destroyAs :: Asteroid -> [Asteroid]
destroyAs (Asteroid pos dir 15) = []
destroyAs (Asteroid pos dir 35) = [(Asteroid pos (dir+40) 15), (Asteroid pos (dir-40) 15)]
destroyAs (Asteroid pos dir 60) = [(Asteroid pos (dir+40) 35), (Asteroid pos (dir-40) 35)]

collision :: (Collidable a, Collidable b) => a -> b -> Bool 
collision a b = collide (hitBox(a)) (hitBox(b))

collide :: HitBox -> HitBox -> Bool 
collide (HitBox (x1,y1) (Circ r1)) (HitBox (x2,y2) (Circ r2)) | sqrt((x1-x2)^2+(y1-y2)^2) < (r1+r2) = True
                                                              | otherwise = False
collide (HitBox (x2,y2) (Rect l b)) (HitBox (x1,y1) (Circ r1)) = distance < r1 
  where lx = x2 - 0.5*b
        rx = x2 + 0.5*b
        ty = y2 + 0.5*l
        by = y2 - 0.5*l  --gives the leftmost and rightmost x coordinate of rectangle, and top and bottom y coordinate.
        closeX = (clamp lx rx x1)  --clamp the x coord of the circle between the x coordinates of the rectangle to obtain the x coordinate of the point on the rectangle that is closest to the circle
        closeY = (clamp by ty y1)  --clamp the y coord of the circle between the y coordinates of the rectangle to obtain the y coordinate of the point on the rectangle that is closest to the circle
        distance = sqrt((x1 - closeX)^2 + (y1 - closeY)^2)  --distance between closest point of rectangle and circle.
collide _ _ = False

clamp :: (Ord a) => a -> a -> a -> a -- help function for our rectangle-circle collision
clamp mn mx = max mn . min mx

getLives :: Ship -> Lives
getLives (Ship _ _ _ _ lives _) = lives



