-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | (isElement (Char 'r') (pressedKeys gstate)) = reload --press r to reload
                 | (lives == 0) = return $ gstate{onScreen = (score, False, True), asteroids = [], enemies = [], bullets =[], ship = (shipDeath(ship gstate))}  --Game Over when running out of lives
                 | paused = return $ gstate  --if paused don't update the gamestate
                 | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = return $ gstate' {ship = move(rotateShip (ship gstate') 8)}  --miss nog achteruit kunnen bewegen?
                 | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = return $ gstate' {ship = move(rotateShip (ship gstate') (-8))}   --evt tegelijkertijd
                 | isElement (Char 'w') (pressedKeys gstate) = return $ gstate' {ship = move(ship gstate')}
                 | (isElement (Char 's') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = return $ gstate' {ship = moveBack(rotateShip (ship gstate') 8)}  
                 | (isElement (Char 's') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = return $ gstate' {ship = moveBack(rotateShip (ship gstate') (-8))}
                 | isElement (Char 's') (pressedKeys gstate) = return $ gstate' {ship = moveBack(ship gstate')}
                 | isElement (Char 'a') (pressedKeys gstate) = return $ gstate' {ship = (rotateShip (ship gstate') 8)}
                 | isElement (Char 'd') (pressedKeys gstate) = return $ gstate' {ship = (rotateShip (ship gstate') (-8))}
                 | otherwise = return $ gstate'
    where gstate' = update gstate{elapsedTime = (elapsedTime gstate) + secs} --bullets = shootAllEn (enemies gstate) (bullets gstate) (elapsedTime gstate)
          (score, paused, _) = (onScreen gstate)
          lives = getLives(ship gstate)

reload :: IO GameState
reload = do asteroids <- mkAsteroids 10
            enemies <- mkEnemies 5
            return (buildInitial asteroids enemies)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input ev gstate = return (inputKey ev gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate {bullets = playerShoot (ship gstate) (bullets gstate)} 
inputKey (EventKey (Char 'p') Down _ _) gstate = gstate {onScreen = (score, not(pause), gameover)}
  where (score, pause, gameover) = (onScreen gstate)
inputKey (EventKey k Down _ _) gstate = gstate {pressedKeys = k:(pressedKeys gstate)}
inputKey (EventKey k Up _ _) gstate = gstate {pressedKeys = (removeItem k (pressedKeys gstate))}
inputKey _ gstate = gstate 
 


