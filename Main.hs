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

----Some graphical elements change over time: animatie in de vorm van een explosie

----Reads and interprets data from the file system, such as high scores or levels, using a simple format:  High scores opslaan


The document discusses the design decisions in the game CHECK
-------------------------
OPTIONAL REQS:
---Different enemies.   NOG WAT EXTRA TOEVOEGEN (LASER ENEMY, om zich heen schieter verschillen in view duidelijk maken, )
Different enemies. Add multiple types of enemies, each of them with different appearance and
substantially different behavior (e.g. just subtracting five rather than four hitpoints on a collision with
an enemy is not sufficiently different). Make the game more interesting by making “harder” enemies
appear later.

--- evt nog een andere uit de lijst van opt. req. Powerups?    POWERUPS ZIJN WEL LACHEN

--- view ietsje mooier maken (bomb sprite voor kamikaze enemy?)

--- invulnerability mode (bij begin spel of als power up)

---------------------------
STYLE REQS:

Do not reuse a single data type for too many things, that is, a single GameObject type or similar
Correct use of sum types – do not shoehorn object-oriented practices
Abstract common interfaces into type classes

Use IO only when necessary
Correct initialization of IO resources

Separate view from logic
Use higher-order functions instead of recursion
Use pattern matching
Do not have magic numbers
Include documentation in the code
-}