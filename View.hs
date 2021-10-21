-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure


viewPure :: GameState -> Picture
viewPure (GameState {asteroids = as, ship = ship, bullets = bs, enemies = es}) = pictures [picAs as, picShip ship, picBs bs, picEs es] where
  picAs :: [Asteroid] -> Picture
  picAs as = pictures (map f as) where
    f :: Asteroid -> Picture
    f (Asteroid (x,y) r sz) = translate x y (color white (rotate (fromIntegral(-r)) (circleSolid (fromIntegral(sz))))) --afhankelijk van de grootte van het scherm. omdat translate werkt vanaf de originele positie van de picture en de picture begint in het midden van het scherm.

  picShip :: Ship -> Picture
  picShip (Ship (x,y) _ r sz _) = translate x y (color blue (rotate (fromIntegral((-r))) (rectangleSolid (fromIntegral(sz)) (0.5*(fromIntegral(sz))))))

  picBs :: [Bullet] -> Picture 
  picBs bs = pictures (map f bs) where
    f :: Bullet -> Picture
    f (Bullet (x,y) _ True) = translate x y (color green (circleSolid 5))  
    f (Bullet (x,y) _ False) = translate x y (color red (circleSolid 5))

  picEs :: [Enemy] -> Picture 
  picEs es = pictures (map f es) where
    f :: Enemy -> Picture
    f (Enemy (x,y) r Shoot) = rotate (fromIntegral(-r)) (translate x y  (color (light red) (rectangleSolid 20 20)))  --later correcte size kiezen
    f (Enemy (x,y) r Follow) = rotate (fromIntegral(-r)) (translate x y (color (dark red) (rectangleSolid 30 30)))