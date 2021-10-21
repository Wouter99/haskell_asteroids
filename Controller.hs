-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = return $ gstate {ship = move(rotateShip (ship gstate) 8), bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map move (enemies gstate)}
                 | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = return $ gstate {ship = move(rotateShip (ship gstate) (-8)), bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map move (enemies gstate)}   --evt tegelijkertijd
                 | isElement (Char 'a') (pressedKeys gstate) = return $ gstate {ship = (rotateShip (ship gstate) 8), bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map move (enemies gstate)} 
                 | isElement (Char 'd') (pressedKeys gstate) = return $ gstate {ship = (rotateShip (ship gstate) (-8)), bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map move (enemies gstate)} 
                 | isElement (Char 'w') (pressedKeys gstate) = return $ gstate {ship = move(ship gstate), bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map move (enemies gstate)}
                 | otherwise = return $ gstate {elapsedTime = elapsedTime gstate + secs, bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map move (enemies gstate)}

                 --miss nog achteruit kunnen bewegen?

--we willen dat hij alle objecten verplaatst van de gamestate, controleert op collisions, en andere gevolgen daarvan afhandelt.


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate {bullets = playerShoot (ship gstate) (bullets gstate)} 
inputKey (EventKey k Down _ _) gstate = gstate { pressedKeys = k:(pressedKeys gstate)}
inputKey (EventKey k Up _ _) gstate = gstate {pressedKeys = (removeItem k (pressedKeys gstate))}
inputKey _ gstate = gstate 


{-
    | c =='w' = gstate{ship = move(ship gstate)}
    | otherwise = gstate
inputKey _ gstate = gstate -}


-- If the user presses the up key, move the ship
--gstate { infoToShow = ShowAChar c }
--inputKey _ gstate = gstate -- Otherwise keep the same


--wat is KeyState: Up/Down?
