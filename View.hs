-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

width :: Int
width = 800

height :: Int
height = 600

view :: GameState -> IO Picture
view = return . viewPure

--vertaling van translate is nog niet helemaal correct. uitzoeken hoe dit werkt.
--(x-0.5*(fromIntegral(width))) (y-0.5*(fromIntegral(height)) i.p.v x y? wss niet want 0,0 is in het midden van de screen.


viewPure :: GameState -> Picture
viewPure (GameState {asteroids = as, ship = ship, bullets = bs, enemies = es}) = pictures [picAs as, picShip ship, picBs bs, picEs es] where
  picAs :: [Asteroid] -> Picture
  picAs as = pictures (map f as) where
    f :: Asteroid -> Picture
    f (Asteroid (x,y) r sz) = rotate (fromIntegral(r)) (translate x y (color white (circleSolid (fromIntegral(sz))))) --afhankelijk van de grootte van het scherm. omdat translate werkt vanaf de originele positie van de picture en de picture begint in het midden van het scherm.

  picShip :: Ship -> Picture
  picShip (Ship (x,y) _ r sz _) = rotate (fromIntegral(r)) (translate x y (color blue (rectangleSolid (0.5*(fromIntegral(sz))) (fromIntegral(sz)))))

  picBs :: [Bullet] -> Picture 
  picBs bs = pictures (map f bs) where
    f :: Bullet -> Picture
    f (Bullet (x,y) _ True) = translate x y (color green (circleSolid 5))  
    f (Bullet (x,y) _ False) = translate x y (color red (circleSolid 5))

  picEs :: [Enemy] -> Picture 
  picEs es = pictures (map f es) where
    f :: Enemy -> Picture
    f (Enemy (x,y) r Shoot) = rotate (fromIntegral(r)) (translate x y  (color (light red) (rectangleSolid 20 20)))  --later correcte size kiezen
    f (Enemy (x,y) r Follow) = rotate (fromIntegral(r)) (translate x y (color (dark red) (rectangleSolid 30 30)))