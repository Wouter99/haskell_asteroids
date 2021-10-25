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
                           asteroids::[Asteroid], 
                           ship::Ship, 
                           bullets::[Bullet],
                           enemies::[Enemy],
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

data Asteroid = Asteroid Position Direction Size  --sizzes zijn 10, 20 of 30. 

data Enemy = Enemy Position Direction Version

data Bullet = Bullet Position Direction Friendly

-- Classes

class Movable a where
  move :: a -> a

class Collidable a where                    --mogelijk de hitbox functie wegwerken. Niet handig omdat we willen pattern matchen.
  hitBox :: a -> (Position, Int)

collision :: Collidable a b => a -> b -> Bool 
collision As@(Asteroid pAs _ szAs) Bs@(Bullet pBs _ True) = collideCircCirc (pAs, szAs) (pBs, 5)
collision (Enemy pEn _ Shoot) (Bullet pBs _ True) = collideCircCirc (pEn, 20) (pBs, 5)
collision (Enemy pEn _ Follow) (Bullet pBs _ True) = collideCircCirc (pEn, 30) (pBs, 5)   --evt later nog size aanpassen
collision (Ship _ _ _) (Bullet _ _ False) = False
collision (Ship _ _ _ _) (Asteroid _ _ _) = False
collision (Ship _ _ _ _) (Enemy _ _ _) = False
collision _ _ = False

collideCircCirc :: (Position,Int) -> (Position, Int) -> Bool
--collision between two circles.
collideCircCirc ((x1,y1) r1) ((x2,y2) r2) | sqrt((x1-x2)^2+(y1-y2)^2) < (r1+r2) = True
                                          | otherwise = False

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


instance Collidable Ship where
  hitBox (Ship p _ _ sz _) = (p,sz)
  --schip is een rectangle van lengte size en breedte 0.5*size

instance Collidable Asteroid where
  hitBox (Asteroid p _ sz) = (p,sz)
  --asteroid is een cirkel met sz = radius

instance Collidable Enemy where 
  hitBox (Enemy p _ Shoot) = (p,20)   
  hitBox (Enemy p _ Follow) = (p,30)

  --enemy wordt vierkant of circle, zien we later wel
  
instance Collidable Bullet where
  hitBox (Bullet p _ _) = (p,5) -- later aanpassen size
  --bullet is cirkel met straal 5


--Alle moveables moeten op een torus zitten!


instance Movable Ship where
  move (Ship pos sp dir sz lives) = Ship (wrap(newPos sp pos dir)) sp dir sz lives  

instance Movable Asteroid where
  move (Asteroid pos dir sz) = Asteroid (wrap(newPos (sizeToSpeed sz) pos dir)) dir sz 
  {-
  move (Asteroid pos dir Small) = Asteroid (newPos 30 pos dir) dir Small --speed aanpassen
  move (Asteroid pos dir Normal) = Asteroid (newPos 20 pos dir) dir Normal --speed aanpassen
  move (Asteroid pos dir Large) = Asteroid (newPos 10 pos dir)  dir Large --speed aanpassen
  -}

instance Movable Enemy where 
  move (Enemy pos dir Shoot) = Enemy (wrap(newPos 3 pos dir)) dir Shoot --speed aanpassen
  move (Enemy pos dir Follow) = Enemy (wrap(newPos 5 pos dir)) dir Follow --speed aanpassen

instance Movable Bullet where
  move (Bullet pos dir fr) = Bullet (newPos 20 pos dir) dir fr   

trans :: Position -> Position
trans (x,y) = (x+ (fromIntegral(width)/2) , y + (fromIntegral(height)/2))

transBack :: Position -> Position
transBack (x,y) = (x- (fromIntegral(width)/2) , y - (fromIntegral(height)/2))

wrap :: Position -> Position
wrap = transBack . wrap' . trans 

wrap' (x,y) = (fromIntegral((round(x-1) `mod` width) + 1), fromIntegral((round(y-1) `mod` height) + 1))

newPos :: Speed -> Position -> Direction -> Position
newPos sp (x,y) dir = (x+sp*cos(degRad*fromIntegral(dir)), y+sp*sin(degRad*fromIntegral(dir)))

sizeToSpeed :: Size -> Speed
sizeToSpeed sz = 60/fromIntegral(sz) --later functie vinden die beter werkt

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

isElement :: Eq a => a -> [a] -> Bool
isElement a [] = False
isElement a (x:xs) = if a == x then True   --if /else veranderen
                     else isElement a xs

rotateShip :: Ship -> Direction -> Ship
rotateShip (Ship pos sp dir sz lives) r = Ship pos sp (dir + r) sz lives

playerShoot :: Ship -> [Bullet] -> [Bullet]
playerShoot (Ship pos sp dir sz lives) bs = (Bullet pos dir True):bs

enemyShoot :: Enemy -> [Bullet] -> [Bullet]
enemyShoot (Enemy pos dir _) bs = (Bullet pos dir False):bs