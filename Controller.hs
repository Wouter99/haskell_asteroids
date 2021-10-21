-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState

{- step secs gstate = map changeShip (filter (\k -> (k == Char 'w' || k == Char 'a' || == Char 'd')) (pressedKeys gstate))
  where changeShip :: Key ->
        changeShip Char 'w' -}

step secs gstate | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = return $ gstate {ship = move(rotateShip (ship gstate) 10), bullets = map move (bullets gstate)}
                 | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = return $ gstate {ship = move(rotateShip (ship gstate) (-10)), bullets = map move (bullets gstate)}   --evt tegelijkertijd
                 | isElement (Char 'a') (pressedKeys gstate) = return $ gstate {ship = (rotateShip (ship gstate) 10), bullets = map move (bullets gstate)} 
                 | isElement (Char 'd') (pressedKeys gstate) = return $ gstate {ship = (rotateShip (ship gstate) (-10)), bullets = map move (bullets gstate)} 
                 | isElement (Char 'w') (pressedKeys gstate) = return $ gstate {ship = move(ship gstate), bullets = map move (bullets gstate)}
                 | otherwise = return $ gstate {elapsedTime = elapsedTime gstate + secs, bullets = map move (bullets gstate)}

--we willen dat hij alle objecten verplaatst van de gamestate, controleert op collisions, en andere gevolgen daarvan afhandelt.

 {- | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }-}


    
{-data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }
-}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey k Down _ _) gstate = gstate { pressedKeys = k:(pressedKeys gstate)}
inputKey (EventKey k Up _ _) gstate = gstate {pressedKeys = (removeItem k (pressedKeys gstate))}
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate {bullets = playerShoot (ship gstate) (bullets gstate) } 
inputKey _ gstate = gstate 


{-
    | c =='w' = gstate{ship = move(ship gstate)}
    | otherwise = gstate
inputKey _ gstate = gstate -}


-- If the user presses the up key, move the ship
--gstate { infoToShow = ShowAChar c }
--inputKey _ gstate = gstate -- Otherwise keep the same


--wat is KeyState: Up/Down?
