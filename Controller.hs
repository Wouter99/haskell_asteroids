-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do gstate' <- reloadState gstate
                      gstate''<- randomSpawner (elapsedTime gstate) $ moveShip $ updateState secs gstate'
                      pauseGameOver gstate gstate''
                      
pauseGameOver :: GameState -> GameState -> IO GameState  
pauseGameOver oGstate gstate | (lives == 0) = do hsc <- readFile "highscore.txt"       --Game over if lives = 0, updates the highscore file and returns the original gamestate without any objects in it. 
                                                 putStrLn hsc -- we do this because we had an issue regarding reading a file, and not using it, before writing. so we came with this fix.
                                                 let highscore = read hsc
                                                 writeFile "highscore.txt" (newScore highscore score)
                                                 return $ oGstate{onScreen = (score, False, True, read(hsc)), asteroids = [], enemies = [], bullets =[], ship = (shipDeath (elapsedTime gstate) (ship gstate))}  --Game Over when running out of lives
                             | paused = return $ oGstate  --if paused this returns the original state that isn't updated
                             | otherwise = return $ gstate
  where (score, paused, _, _) = (onScreen gstate)
        lives = getLives(ship gstate)

newScore :: Int -> Int -> String -- checks if the current score is a highscore
newScore hsc sc | hsc >= sc = show(hsc)     
                | otherwise = show(sc)

reloadState :: GameState -> IO GameState
reloadState gstate | (isElement (Char 'r') (pressedKeys gstate)) = reload --press r to reload the game
                   | otherwise = return $ gstate
                   
moveShip :: GameState -> GameState -- handles all key combinations for moving the ship
moveShip gstate | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = gstate {ship = move(rotateShip (ship gstate) 8)}  
                | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = gstate {ship = move(rotateShip (ship gstate) (-8))}  
                | isElement (Char 'w') (pressedKeys gstate) = gstate {ship = move(ship gstate)}
                | (isElement (Char 's') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = gstate {ship = moveBack(rotateShip (ship gstate) 8)}  
                | (isElement (Char 's') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = gstate {ship = moveBack(rotateShip (ship gstate) (-8))}
                | isElement (Char 's') (pressedKeys gstate) = gstate {ship = moveBack(ship gstate)}
                | isElement (Char 'a') (pressedKeys gstate) = gstate {ship = (rotateShip (ship gstate) 8)}
                | isElement (Char 'd') (pressedKeys gstate) = gstate {ship = (rotateShip (ship gstate) (-8))}
                | otherwise = gstate

updateState :: Float -> GameState -> GameState 
updateState secs gstate = update gstate{elapsedTime = (elapsedTime gstate) + secs} 
          
randomSpawner :: Float -> GameState -> IO GameState -- these values were tested and were considered OK. they could be change very easily.
randomSpawner time gstate | time < 10 = spawnWithChance 1.003 1.0003 gstate 
                          | time < 30 = spawnWithChance 1.005 1.005 gstate 
                          | time < 50 = spawnWithChance 1.007 1.006 gstate 
                          | time < 80 = spawnWithChance 1.009 1.008 gstate 
                          | time < 120 = spawnWithChance 1.009 1.009 gstate
                          | otherwise = spawnWithChance 1.01 1.01 gstate

--p1 and p2 are numbers a tiny bit larger than 1, such that if the random number drawn between 0 and p1/p2 is larger than 1,
--it makes one asteroid/enemy, otherwise it makes 0 asteroids/enemies this step
spawnWithChance :: Float -> Float -> GameState -> IO GameState
spawnWithChance p1 p2 gstate = do f1 <- randomRIO(0,p1) :: IO Float  
                                  let n1 = floor(f1) 
                                  as <- mkAsteroids n1   
                                  f2 <- randomRIO(0,p2) :: IO Float 
                                  let n2 = floor(f2) 
                                  es <- mkEnemies n2  
                                  return gstate {asteroids = ((asteroids gstate)++as), enemies = ((enemies gstate)++es)}                        

reload :: IO GameState --resets the game
reload = do asteroids <- mkAsteroids 7
            enemies <- mkEnemies 1
            return (buildInitial asteroids enemies)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input ev gstate = return (inputKey ev gstate)

--handles the input keys from the user. Pattern matches on spacebar and p for shooting and pausing. Any other keys are added to the pressedKeys list and the effects of those are handled in the step function.
inputKey :: Event -> GameState -> GameState 
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate {bullets = playerShoot (ship gstate) (bullets gstate)} 
inputKey (EventKey (Char 'p') Down _ _) gstate = gstate {onScreen = (score, not(pause), gameover, score)}
  where (score, pause, gameover, highscore) = (onScreen gstate)
inputKey (EventKey k Down _ _) gstate = gstate {pressedKeys = k:(pressedKeys gstate)}
inputKey (EventKey k Up _ _) gstate = gstate {pressedKeys = (removeItem k (pressedKeys gstate))}
inputKey _ gstate = gstate 
 


