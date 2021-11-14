-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState {asteroids = as, ship = ship, bullets = bs, enemies = es, onScreen = info, elapsedTime = time}) = pictures [picAs as, picShip ship, picBs bs, picEs ship es, picInfo ship info, picExplosion time ship] where
  picAs :: [Asteroid] -> Picture
  picAs as = pictures (map f as) where
    f :: Asteroid -> Picture
    f (Asteroid (x,y) r sz) = translate x y (color white (rotate (-r) (circleSolid (sz)))) 

  picShip :: Ship -> Picture -- draws the ship
  picShip (Ship (x,y) _ r sz _ _) = translate x y (color blue (rotate (-r) (rectangleSolid (sz) (0.5*sz))))

  picExplosion :: Float -> Ship -> Picture -- draws an explosion, which consists of multiple colored circles changing in radius
  picExplosion _ (Ship _ _ _ _ _ None) = Blank
  picExplosion totalTime (Ship _ _ _ _ _ (Explosion (x,y) creationTime)) | (duration < 0.3) = Pictures [translate x y (color orange (circleSolid (160*duration))), translate x y (color yellow (circleSolid (120*duration))), translate x y (color white (circleSolid (80*duration)))]
                                                                         | otherwise = Pictures [translate x y (color orange (circleSolid (1.6/(1000*duration)))), translate x y (color orange (circleSolid (1.2/(1000*duration)))), translate x y (color orange (circleSolid (0.8/(1000*duration))))]
    where duration = totalTime - creationTime

  picBs :: [Bullet] -> Picture -- draws the bullets
  picBs bs = pictures (map f bs) where
    f :: Bullet -> Picture
    f (Bullet (x,y) _ True) = translate x y (color green (circleSolid 5))  
    f (Bullet (x,y) _ False) = translate x y (color red (circleSolid 5))

  picEs :: Ship -> [Enemy] -> Picture -- draws the enemies
  picEs ship es = pictures (map (f ship) es) where
    f :: Ship -> Enemy -> Picture
    f (Ship posShip _ _ _ _ _) (Enemy (x,y) r Target) = Pictures [translate x y (color orange (thickCircle 20 6)), translate x y (color orange (rotate (-dirP) (rectangleSolid (45) (10))))]
      where dirP = dirPlayer (x,y) posShip
    f _ (Enemy (x,y) r Shoot) = translate x y (color (light red) (rotate (-r) (circleSolid (30))))  
    f _ (Enemy pos dir Follow) = Pictures [makeRect dir pos, makeRect (dir+45) pos, makeRect (dir+90) pos, makeRect (dir+135) pos]  --is depicted as a bunch of rotated rectangles such that is also has a circular hitbox
 
  makeRect :: Direction -> Position -> Picture
  makeRect dir (x,y) = translate x y (color (dark red) (rotate (-dir) (rectangleSolid (80) (15))))

  picInfo :: Ship -> (Score, Pause, GameOver, Highscore) -> Picture -- creates the user interface (displays the score, lives of the player, and other text)
  picInfo _ (score,_,True,highscore) = Pictures [translate (-0.49*(fromIntegral(width))) 0 (scale 1.5 1.5 (color (white) (text "GAME OVER"))), translate (-0.42*(fromIntegral(width))) (-150) (scale 0.5 0.5 (color (white) (text "press R to restart"))), translate (-0.42*(fromIntegral(width))) (-275) (scale 0.5 0.5 (color (white) (text ("You scored "++(show(score))++" points" )))), translate (-0.42*(fromIntegral(width))) (-350) (scale 0.5 0.5 (color (white) (text ("Current highscore: "++(show(highscore))++" points" ))))]
  picInfo _ (_,True,_,_) = translate (-0.42*(fromIntegral(width))) 0 (scale 2 2 (color (white) (text "PAUSED")))
  picInfo (Ship _ _ _ _ lives _) (score, False,_,_) = Pictures [translate (-0.5*(fromIntegral(width))+20) (0.5*(fromIntegral(height))-50) (scale 0.4 0.4 (color (white) (text ("Score: "++(show score))))), translate (-0.5*(fromIntegral(width))+20) (0.5*(fromIntegral(height))-100) (scale 0.4 0.4 (color (white) (text ("Lives: "++(show lives)))))]