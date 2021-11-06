module Main where

import Controller
import Model
import View
import Data.List

import Graphics.Gloss.Interface.IO.Game

main :: IO ()

main = do asteroids <- mkAsteroids 7
          enemies <- mkEnemies 4
          playIO (InWindow "Asteroids" (width, height) (0, 0)) -- Or FullScreen
                black            -- Background color
                30               -- Frames per second
                (buildInitial asteroids enemies)  -- Initial state, built with the random values created in Main.
                view             -- View function
                input            -- Event function
                step             -- Step function
                

 

{- 
TODO LIJSTJE 

----------------------------
MINIMAL REQS:
Drawing the player on the screen  CHECK
Control the player with the keyboard  CHECK
Enemies appear or move randomly  CHECK
The player can pause and resume the game with a key. There is visual indication of the game being paused: CHECK
Evolution and movement over time: zorgen dat er asteroids en enemies blijven spawnen en dat het langzaam moeilijker wordt CHECK
(Some) enemies work “intelligently” towards their goal: verschillende enemy ai's: CHECK
Some graphical elements change over time: animatie in de vorm van een explosie CHECK
Reads and interprets data from the file system, such as high scores or levels, using a simple format:  

---manier vinden om niet elke step de score te printen WOUTER
---view highscore WOUTER
---tweaken spel WOUTER
---design doc updaten WOUTER
-}