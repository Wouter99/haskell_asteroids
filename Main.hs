module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Asteroids" (width, height) (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function


{- 
TODO LIJSTJE 

fixen van juiste coordinaten bij het tekenen
beweging moet werken op een torus
step functie implementeren zodat de asteroids en enemies autonoom kunnen bewegen.

input functie implementeren voor de ship en het schieten van bullets
functionaliteit van het bullets schieten
collision implementeren en laten controleren in de step functie. Gevolgen van collisions uitwerken (levens schip minder, bullet die verdwijnt, enemie die verdwijnt, asteroid die in kleinere stukken kapotgaat of verdwijnt)

werkende vijanden implementeren (beginnen met de basis enemy, deze moet kunnen schieten. later kijken of die ook nog anders dan een rechte lijn gaat bewegen.)
werkende score functie maken
rekening houden met niet oneindig veel data types (wat lagg veroorzaakt) 
goede user interface zoals beschreven in game design
pauze functie maken
sprites plaatsen op de shapes

-}  