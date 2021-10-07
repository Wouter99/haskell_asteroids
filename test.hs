module Main where

import Data.Char
import Data.List
import Data.Maybe

main = putStrLn "Hello, world!"

data Direction = North | South | East | West

directionName :: Direction -> String

data Point = Pt Float Float
-- data Point = Point Float Float mag ook

uncurry :: (a -> b -> c) -> (a,b) -> c 
uncurry f (x,y) = f x y
uncurry f       = \(x,y) -> f x y

data IntList  = EmptyList | Cons Int IntList
data IntTree = EmptyTree | Node Int IntTree IntTree

data Tree a = Leaf | Node (Tree a) a (Tree a)

--elemTree :: a -> Tree -> Bool
--elemTree _ Leaf = False
--elemTree a (Node l x r) | a == x = True
--                        | elemTree a 


--class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool

--instance Eq Point where
--    Pt x y == Pt u v = x == u && y == v
--    Pt x y /= Pt u v = x /= u || y /= v

multipleFS :: Int -> Bool
multipleFS x = x%5 == 0 || x%7 == 0

