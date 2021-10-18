-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

degRad :: Float
degRad = 0.0174532925

data GameState = GameState{
                           asteroids::[Asteroid], 
                           ship::Ship, 
                           bullets::[Bullet],
                           enemies::[Enemy],
                           elapsedTime :: Float
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
initialState :: GameState
initialState = GameState {asteroids = [Asteroid (100,100) 45 20], ship = Ship (300,300) 0 100 20 3, bullets = [], enemies = [Enemy (400,400) 150 Shoot], elapsedTime = 0}

-- Main datatypes
data Ship = Ship Position Speed Direction Size Lives

data Asteroid = Asteroid Position Direction Size

data Enemy = Enemy Position Direction Version

data Bullet = Bullet Position Direction Friendly

-- Classes

class Movable a where
  move :: a -> a

class Collidable a where
  hitBox :: a -> (Position, Int)

--collision :: Collidable a,b => a -> b -> Bool

{-  HIER STAAN X EN Y VOOR HET MIDDELPUNT VAN HET OBJECT
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
  {-
  hitBox (Ship p _ _ Small _) = (p,10)  --later daadwerkelijke size kiezen. wordt eventueel veranderd door powerups
  hitBox (Ship p _ _ Normal _) = (p,20)
  hitBox (Ship p _ _ Large _) = (p,30)
  -}
 
instance Collidable Asteroid where
  hitBox (Asteroid p _ sz) = (p,sz)
  {-
  hitBox (Asteroid p _ Small) = (p,10)  --size aanpasseen
  hitBox (Asteroid p _ Normal) = (p,20)
  hitBox (Asteroid p _ Large) = (p,30)
  -}

instance Collidable Enemy where 
  hitBox (Enemy p _ Shoot) = (p,20)   --later size aanpassen
  hitBox (Enemy p _ Follow) = (p,30)
  
instance Collidable Bullet where
  hitBox (Bullet p _ _) = (p,5) -- later aanpassen size


--Alle moveables moeten op een torus zitten!

sizeToSpeed :: Size -> Speed
sizeToSpeed sz = fromIntegral(sz) --later functie vinden die beter werkt

instance Movable Ship where
  move (Ship pos sp dir sz lives) = Ship (newPos 10 pos dir) sp dir sz lives  --miss record structuur?  --miss ander syntax met sinAngle en cosAngle 

instance Movable Asteroid where
  move (Asteroid pos dir sz) = Asteroid (newPos (sizeToSpeed sz) pos dir) dir sz 
  {-
  move (Asteroid pos dir Small) = Asteroid (newPos 30 pos dir) dir Small --speed aanpassen
  move (Asteroid pos dir Normal) = Asteroid (newPos 20 pos dir) dir Normal --speed aanpassen
  move (Asteroid pos dir Large) = Asteroid (newPos 10 pos dir)  dir Large --speed aanpassen
  -}

instance Movable Enemy where 
  move (Enemy pos dir Shoot) = Enemy (newPos 20 pos dir) dir Shoot --speed aanpassen
  move (Enemy pos dir Follow) = Enemy (newPos 20 pos dir) dir Follow --speed aanpassen

instance Movable Bullet where
  move (Bullet pos dir fr) = Bullet (newPos 50 pos dir) dir fr --speed aanpassen
 

newPos :: Speed -> Position -> Direction -> Position
newPos sp (x,y) dir = (x+sp*cos(degRad*fromIntegral(dir)), y+sp*sin(degRad*fromIntegral(dir)))