module Main where

import Controller
import Model
import View
import System.Random
import Data.List

import Graphics.Gloss.Interface.IO.Game

main :: IO ()

mkAsteroid :: (Float,Float,Int,Int) -> Asteroid
mkAsteroid (x,y,dir,size) = Asteroid (x,y) dir size

buildInitial :: [(Float,Float,Int,Int)] -> GameState
buildInitial asvals = GameState {asteroids = map mkAsteroid asvals, ship = Ship (0,0) 5 90 30 3, bullets = [], enemies = [Enemy (150,-100) 45 Shoot], elapsedTime = 0, pressedKeys = []}

main = do randxs <- sequence $ replicate 5 $ randomRIO(-350, 350)
          randys <- sequence $ replicate 5 $ randomRIO(-250, 250)
          randDirs <- sequence $ replicate 5 $ randomRIO(0, 360)
          randInts <- sequence $ replicate 5 $ randomRIO(1,3)
          let randSizes = map (10*) randInts
          let asteroidValues = zip4 randxs randys randDirs randSizes
          playIO (InWindow "Asteroids" (width, height) (0, 0)) -- Or FullScreen
                black            -- Background color
                30               -- Frames per second
                (buildInitial asteroidValues)  -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function

--beter uitwerken van de random initial gamestate. Lijst van 5 asteroids met random pos, dir, en size. 


{- 
TODO LIJSTJE 

beweging moet werken op een torus
zorgen dat de asteroids en enemies autonoom kunnen bewegen.

collision implementeren en laten controleren in de step functie. Gevolgen van collisions uitwerken (levens schip minder, bullet die verdwijnt, enemie die verdwijnt, asteroid die in kleinere stukken kapotgaat of verdwijnt)

werkende vijanden implementeren (beginnen met de basis enemy, deze moet kunnen schieten. later kijken of die ook nog anders dan een rechte lijn gaat bewegen.)
werkende score functie maken
levens weergeven op scherm
rekening houden met niet oneindig veel data types (wat lagg veroorzaakt) 
goede user interface zoals beschreven in game design
pauze functie maken
sprites plaatsen op de shapes

-}  