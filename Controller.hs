-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = return $ gstate' {ship = move(rotateShip (ship gstate) 8)}  --miss nog achteruit kunnen bewegen?
                 | (isElement (Char 'w') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = return $ gstate' {ship = move(rotateShip (ship gstate) (-8))}   --evt tegelijkertijd
                 | isElement (Char 'w') (pressedKeys gstate) = return $ gstate' {ship = move(ship gstate)}
                 --  Willen we kunnen draaien bij achteruit beweging?
                 | (isElement (Char 's') (pressedKeys gstate) && isElement (Char 'a') (pressedKeys gstate)) = return $ gstate' {ship = moveBack(rotateShip (ship gstate) 8)}  
                 | (isElement (Char 's') (pressedKeys gstate) && isElement (Char 'd') (pressedKeys gstate)) = return $ gstate' {ship = moveBack(rotateShip (ship gstate) (-8))}
                   
                 | isElement (Char 's') (pressedKeys gstate) = return $ gstate' {ship = moveBack(ship gstate)}
                 | isElement (Char 'a') (pressedKeys gstate) = return $ gstate' {ship = (rotateShip (ship gstate) 8)}
                 | isElement (Char 'd') (pressedKeys gstate) = return $ gstate' {ship = (rotateShip (ship gstate) (-8))}
                 | otherwise = return $ gstate'
    where gstate' = update gstate{elapsedTime = (elapsedTime gstate) + secs}
                
--we willen dat de update functie alle objecten verplaatst van de gamestate, controleert op collisions, en andere gevolgen daarvan afhandelt.
update :: GameState -> GameState
update gstate =  handleCollisions (moveAll gstate)

--------------------------------
--helper functions to update the gamestate
--------------------------------

moveAll :: GameState -> GameState
moveAll gstate = gstate {bullets = map move (bullets gstate), asteroids = map move (asteroids gstate), enemies = map move (enemies gstate)}
    
handleCollisions :: GameState -> GameState 
handleCollisions (GameState as ship bs es time keys) = GameState as' ship' bs' es' time keys          
  where      
    (as', ship', bs', es') = collisions as ship bs es         --misschien hier nog checken dat er maximaal 1 leven wordt afgenomen van het schip per step? misschien nog invulnerability implementeren.

collisions :: [Asteroid] -> Ship -> [Bullet] -> [Enemy] -> ([Asteroid], Ship, [Bullet], [Enemy])        --evt hele gamestate meegeven
collisions as ship bs es = (as'', ship''', bs',es'')
  where
    (as', ship', bs', es') = bulletChecker as ship bs es 
    (as'', ship'') = asteroidChecker as' ship'
    (es'', ship''') = enemyChecker es' ship''


    
bulletChecker :: [Asteroid] -> Ship -> [Bullet] -> [Enemy] -> ([Asteroid], Ship, [Bullet], [Enemy])
bulletChecker as ship bs es = (as', ship', bs''', es')
  where (as', bs') = bulletAsChecker as bs
        (es', bs'') = bulletEnChecker es bs'
        (ship', bs''') = bulletShipChecker ship bs''


bulletEnChecker :: [Enemy] -> [Bullet] -> ([Enemy], [Bullet])
bulletEnChecker [] bs = ([], bs)
bulletEnChecker (e:es) bs = ((es'++esr), bsr)
  where (es', bs') = bulletEnChecker' e bs
        (esr, bsr) = bulletEnChecker es bs'

bulletEnChecker' :: Enemy -> [Bullet] -> ([Enemy], [Bullet])
--Als bullets leeg zijn dan klaar
bulletEnChecker' e [] = ([e], [])  
bulletEnChecker' e (b:bs) | collision e b = ([], bs)
                          | otherwise = let (es', bs') = (bulletEnChecker' e bs) in (es', (b:bs'))



bulletAsChecker :: [Asteroid] -> [Bullet] -> ([Asteroid], [Bullet])
bulletAsChecker [] bs = ([], bs)
bulletAsChecker (a:as) bs = ((as'++asr), bsr)
  where (as', bs') = bulletAsChecker' a bs
        (asr, bsr) = bulletAsChecker as bs'

bulletAsChecker':: Asteroid -> [Bullet] -> ([Asteroid], [Bullet])
--Als bullets leeg zijn dan klaar
bulletAsChecker' a [] = ([a], [])  
bulletAsChecker' a (b:bs) | collision a b = (destroyAs(a), bs)
                          | otherwise = let (as', bs') = (bulletAsChecker' a bs) in (as', (b:bs'))


bulletShipChecker :: Ship -> [Bullet] -> (Ship, [Bullet])
bulletShipChecker ship [] = (ship, [])
bulletShipChecker ship (b:bs) | collision ship b = ((shipDeath(ship), bs))  
                              | otherwise = let (ship', bs') = (bulletShipChecker ship bs) in (ship', b:bs')

asteroidChecker :: [Asteroid] -> Ship -> ([Asteroid], Ship)
asteroidChecker [] ship = ([], ship)
asteroidChecker (a:as) ship | collision ship a = let (as', ship') = (asteroidChecker as (shipDeath(ship))) in ((destroyAs(a)++as'), ship') --respawn functie!
                            | otherwise = let (as', ship') = (asteroidChecker as ship) in ((a:as'),ship')

enemyChecker :: [Enemy] -> Ship -> ([Enemy], Ship)
enemyChecker [] ship = ([], ship)
enemyChecker (e:es) ship | collision ship e = enemyChecker es (shipDeath(ship))     
                         | otherwise = let (es', ship') = (enemyChecker es ship) in ((e:es'), ship')                

shipDeath :: Ship -> Ship
shipDeath (Ship pos sp dir sz 0) = Ship (100000000,0) sp dir sz 0  -- beter alternatief bedenken
shipDeath (Ship pos sp dir sz lv) = Ship (0,0) sp dir sz (lv-1) 

destroyAs :: Asteroid -> [Asteroid]
destroyAs (Asteroid pos dir 10) = []
destroyAs (Asteroid pos dir 20) = [(Asteroid pos (dir+25) 10), (Asteroid pos (dir-25) 10)]
destroyAs (Asteroid pos dir 30) = [(Asteroid pos (dir+25) 20), (Asteroid pos (dir-25) 20)]


collision :: (Collidable a, Collidable b) => a -> b -> Bool 
collision a b = collide (hitBox(a)) (hitBox(b))

collide :: HitBox -> HitBox -> Bool 
collide (HitBox (x1,y1) (Circ r1)) (HitBox (x2,y2) (Circ r2)) | sqrt((x1-x2)^2+(y1-y2)^2) < (fromIntegral(r1+r2)) = True
                                                              | otherwise = False
collide (HitBox (x2,y2) (Rect l b)) (HitBox (x1,y1) (Circ r1)) = False
collide _ _ = False


-- | Handle user input
input :: Event -> GameState -> IO GameState
input ev gstate = return (inputKey ev gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate {bullets = playerShoot (ship gstate) (bullets gstate)} 
inputKey (EventKey k Down _ _) gstate = gstate { pressedKeys = k:(pressedKeys gstate)}
inputKey (EventKey k Up _ _) gstate = gstate {pressedKeys = (removeItem k (pressedKeys gstate))}
inputKey _ gstate = gstate 


