-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure


viewPure :: GameState -> Picture
viewPure (GameState {asteroids = as, ship = ship, bullets = bs, enemies = es, onScreen = info}) = pictures [picAs as, picShip ship, picBs bs, picEs es, picInfo ship info] where
  picAs :: [Asteroid] -> Picture
  picAs as = pictures (map f as) where
    f :: Asteroid -> Picture
    f (Asteroid (x,y) r sz) = translate x y (color white (rotate (-r) (circleSolid (sz)))) --afhankelijk van de grootte van het scherm. omdat translate werkt vanaf de originele positie van de picture en de picture begint in het midden van het scherm.

  picShip :: Ship -> Picture
  picShip (Ship (x,y) _ r sz _) = translate x y (color blue (rotate (-r) (rectangleSolid (sz) (0.5*sz))))

  picBs :: [Bullet] -> Picture 
  picBs bs = pictures (map f bs) where
    f :: Bullet -> Picture
    f (Bullet (x,y) _ True) = translate x y (color green (circleSolid 5))  
    f (Bullet (x,y) _ False) = translate x y (color red (circleSolid 5))

  picEs :: [Enemy] -> Picture 
  picEs es = pictures (map f es) where
    f :: Enemy -> Picture
    f (Enemy (x,y) r Shoot) = translate x y (color (light red) (rotate (-r) (circleSolid (20))))  --later correcte size kiezen
    f (Enemy (x,y) r Follow) = translate x y (color (dark red) (rotate (-r) (circleSolid (30))))

  picInfo :: Ship -> (Score, Pause, GameOver) -> Picture
  picInfo _ (score,_,True) = Pictures [translate (-0.49*(fromIntegral(width))) 0 (scale 1.5 1.5 (color (white) (text "GAME OVER"))), translate (-0.42*(fromIntegral(width))) (-200) (scale 0.5 0.5 (color (white) (text "press R to restart"))), translate (-0.42*(fromIntegral(width))) (-300) (scale 0.5 0.5 (color (white) (text ("You scored "++(show(score))++" points" ))))]
  picInfo _ (_,True,_) = translate (-0.42*(fromIntegral(width))) 0 (scale 2 2 (color (white) (text "PAUSED")))
  picInfo (Ship _ _ _ _ lives) (score, False,_) = Pictures [translate (-0.5*(fromIntegral(width))+20) (0.5*(fromIntegral(height))-50) (scale 0.4 0.4 (color (white) (text ("Score: "++(show score))))), translate (-0.5*(fromIntegral(width))+20) (0.5*(fromIntegral(height))-100) (scale 0.4 0.4 (color (white) (text ("Lives: "++(show lives)))))]