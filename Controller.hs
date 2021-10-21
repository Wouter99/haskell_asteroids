-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate {elapsedTime = elapsedTime gstate + secs}

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
inputKey (EventKey (Char c) Down _ _) gstate 
    | c =='w' = gstate{ship = move(ship gstate)}
    | otherwise = gstate
inputKey _ gstate = gstate


-- If the user presses the up key, move the ship
--gstate { infoToShow = ShowAChar c }
--inputKey _ gstate = gstate -- Otherwise keep the same


--wat is KeyState: Up/Down?
