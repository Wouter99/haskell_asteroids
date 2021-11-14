-- Made by Wouter van den Berg and Fedor Taggenbrock 

module Main where

import Controller
import Model
import View
import Data.List

import Graphics.Gloss.Interface.IO.Game

main :: IO ()

main = do asteroids <- mkAsteroids 7
          enemies <- mkEnemies 1
          playIO (InWindow "Asteroids" (width, height) (0, 0)) -- Or FullScreen
                black            -- Background color
                30               -- Frames per second
                (buildInitial asteroids enemies)  -- Initial state, built with the random values created in Main.
                view             -- View function
                input            -- Event function
                step             -- Step function