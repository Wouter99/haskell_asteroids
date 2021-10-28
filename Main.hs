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

mkEnemy :: (Float,Float,Int,Int) -> Enemy
mkEnemy (x,y,dir,1) = Enemy (x,y) dir Follow
mkEnemy (x,y,dir,2) = Enemy (x,y) dir Shoot


buildInitial :: [(Float,Float,Int,Int)] -> [(Float,Float,Int,Int)] -> GameState
buildInitial asVals enVals = GameState {asteroids = map mkAsteroid asVals, ship = Ship (0,0) 5 90 30 3, bullets = [], enemies = map mkEnemy enVals , elapsedTime = 0, pressedKeys = []}

main = do posAs <- createRandomPos 10  --creating the random values for asteroids
          let (xAs, yAs) = unzip posAs 
          randDirsAs <- createRandomDir 10
          randIntsAs <- sequence $ replicate 10 $ randomRIO(1,3)
          let randSizesAs = map (10*) randIntsAs          --all asteroids have sizes 10, 20 or 30

          posEn <-createRandomPos 5 --creating the random values for enemies
          let (xEn, yEn) = unzip posEn
          randDirsEn <- createRandomDir 5
          randVersEn <- sequence $ replicate 5 $ randomRIO(1,2)  --meerdere enemies kunnen later toegevoegd worden
          
          let asteroidValues = zip4 xAs yAs randDirsAs randSizesAs
          let enemyValues = zip4 xEn yEn randDirsEn randVersEn
          
          playIO (InWindow "Asteroids" (width, height) (0, 0)) -- Or FullScreen
                black            -- Background color
                30               -- Frames per second
                (buildInitial asteroidValues enemyValues)  -- Initial state, built with the random values created in Main.
                view             -- View function
                input            -- Event function
                step             -- Step function


createRandomPos :: Int ->  IO [Position]
createRandomPos n = do randxs <- sequence $ replicate n $ randomRIO((-width) `div` 2 , width `div` 2)
                       randys <- sequence $ replicate n $ randomRIO((-height) `div` 2, height `div` 2)
                       return (zip (map fromIntegral randxs) (map fromIntegral randys))

createRandomDir :: Int -> IO [Int]
createRandomDir n = (sequence $ replicate n $ randomRIO(0, 360))

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