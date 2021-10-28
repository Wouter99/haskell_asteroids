-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width :: Int
width = 800

height :: Int
height = 600

--nO_SECS_BETWEEN_CYCLES :: Float
--nO_SECS_BETWEEN_CYCLES = 5

degRad :: Float
degRad = 0.0174532925

data GameState = GameState{
                           asteroids :: [Asteroid], 
                           ship :: Ship, 
                           bullets :: [Bullet],
                           enemies :: [Enemy],
                           elapsedTime :: Float,
                           pressedKeys :: [Key]
                          } 

-- Auxiliary datatypes

type Position = (Float, Float) --representeert het middelpunt van het object

type Speed = Float 

type Size = Int  -- 10, 20 of 30

type Friendly = Bool

type Direction = Int --measured in degrees. can be converted to radians with degRad. 

data Version = Shoot | Follow

type Lives = Int

-- initalState willen we random, maar dan moet het een IO GameState worden. Is dat een probleem?

-- Main datatypes
data Ship = Ship Position Speed Direction Size Lives

data Asteroid = Asteroid Position Direction Size  --sizes zijn 10, 20 of 30. 

data Enemy = Enemy Position Direction Version

data Bullet = Bullet Position Direction Friendly

data HitBox = HitBox Position Shape

data Shape = Rect Int Int | Circ Int


class Movable a where
  move :: a -> a

class Collidable a where
  hitBox :: a -> HitBox

instance Collidable Ship where
  hitBox (Ship pos sp dir sz lives) = HitBox pos (Rect sz (sz `div` 2))
 
instance Collidable Asteroid where
  hitBox (Asteroid p _ sz) = HitBox p (Circ sz)

instance Collidable Enemy where 
  hitBox (Enemy p _ Shoot) = HitBox p (Circ 20)   --later size aanpassen
  hitBox (Enemy p _ Follow) = HitBox p (Circ 30)
  
instance Collidable Bullet where
  hitBox (Bullet p _ _) = HitBox p (Circ 5) -- later aanpassen size


instance Movable Ship where
  move (Ship pos sp dir sz lives) = Ship (wrap(newPos sp pos dir)) sp dir sz lives  

instance Movable Asteroid where
  move (Asteroid pos dir sz) = Asteroid (wrap(newPos (sizeToSpeed sz) pos dir)) dir sz 

instance Movable Enemy where 
  move (Enemy pos dir Shoot) = Enemy (wrap(newPos 3 pos dir)) dir Shoot --speed aanpassen
  move (Enemy pos dir Follow) = Enemy (wrap(newPos 5 pos dir)) dir Follow --speed aanpassen

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
newPos sp (x,y) dir = (x+sp*cos(degRad*fromIntegral(dir)), y+sp*sin(degRad*fromIntegral(dir)))

sizeToSpeed :: Size -> Speed
sizeToSpeed sz = 60/fromIntegral(sz) --later functie vinden die beter werkt

moveBack :: Ship -> Ship
moveBack (Ship (x,y) sp dir sz lives) = Ship (wrap((x-sp*cos(degRad*fromIntegral(dir)), y-sp*sin(degRad*fromIntegral(dir))))) sp dir sz lives

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
rotateShip (Ship pos sp dir sz lives) r = Ship pos sp (dir + r) sz lives

playerShoot :: Ship -> [Bullet] -> [Bullet]
playerShoot (Ship pos sp dir sz lives) bs = (Bullet pos dir True):bs

enemyShoot :: Enemy -> [Bullet] -> [Bullet]
enemyShoot (Enemy pos dir _) bs = (Bullet pos dir False):bs



{-
collision a@(Asteroid _ _ _) b@(Bullet _ _ _) = collideCircCirc (hitBox(a)) (hitBox(b))
collision e@(Enemy pEn _ Shoot) b@(Bullet pBs _ True) = collideCircCirc (hitBox(e)) (hitBox(b))
collision (Enemy pEn _ Follow) (Bullet pBs _ True) = collideCircCirc (pEn, 30) (pBs, 5)   --evt later nog size aanpassen
collision (Ship _ _ _ _ _) (Bullet _ _ False) = collideRectCirc hitBox(ship) hitBox(bullet)
collision (Ship _ _ _ _ _) (Asteroid _ _ _) = False
collision (Ship _ _ _ _ _) (Enemy _ _ _) = False
collision _ _ = False


collideCircCirc :: (Position,Int) -> (Position, Int) -> Bool
--collision between two circles.
collideCircCirc ((x1,y1), r1) ((x2,y2), r2) | sqrt((x1-x2)^2+(y1-y2)^2) < (r1+r2) = True
                                            | otherwise = False

--collideRectCirc :: (Position,Int) -> (Position, Int) -> Bool
-}

{-
--HIER STAAN X EN Y VOOR HET MIDDELPUNT VAN HET OBJECT
collision a b = (abs(x - x’) < sizex ha hb) && (abs(y - y’) sizey ha hb) -- onder voorbehoud
  where ha@((x,y), s) = hitBox a
    hb@((x’,y’), s’) = hitBox b
    sizex ((x,y), s) ((x’,y’), s’) | x>x’ = s
                                   | otherwise = s’ 
    sizey ((x,y), s) ((x’,y’), s’) | y>y’ = s
                                   | otherwise = s’ 
-}