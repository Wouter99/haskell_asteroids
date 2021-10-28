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
bulletChecker [] ship bs es = ([], ship, bs, es)
bulletChecker as ship bs [] = (as, ship, bs, [])
bulletChecker (a:as) ship bs (e:es) = ((as'++asr), shipr, bsr, esr)
  where (as', bs') = bulletAsChecker a bs
        (asr, shipr, bsr, esr) = bulletChecker as ship bs' (e:es)
  

{-
   ((as'++as''), ship'', bs'', es)
  where (as' ,ship',bs',es') = (bulletChecker' a ship bs e)
        (as'', ship'', bs'',es'') = (bulletChecker as ship' bs' es)
-}


bulletEnChecker :: Enemy -> [Bullet] -> ([Enemy], [Bullet])
--Als bullets leeg zijn dan klaar
bulletEnChecker e [] = ([e], [])  
bulletEnChecker e (b:bs) | collision e b = ([], bs)
                         | otherwise = let (es', bs') = (bulletEnChecker e bs) in (es', (b:bs'))

bulletAsChecker :: Asteroid -> [Bullet] -> ([Asteroid], [Bullet])
--Als bullets leeg zijn dan klaar
bulletAsChecker a [] = ([a], [])  
bulletAsChecker a (b:bs) | collision a b = (destroyAs(a), bs)
                         | otherwise = let (as', bs') = (bulletAsChecker a bs) in (as', (b:bs'))

bulletShipChecker :: Ship -> [Bullet] -> (Ship, [Bullet])
bulletShipChecker ship [] = (ship, [])
bulletShipChecker ship (b:bs) | collision ship b = ((shipDeath(ship), bs))  
                              | otherwise = bulletShipChecker ship bs

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


{-
    | c =='w' = gstate{ship = move(ship gstate)}
    | otherwise = gstate
inputKey _ gstate = gstate -}


-- If the user presses the up key, move the ship
--gstate { infoToShow = ShowAChar c }
--inputKey _ gstate = gstate -- Otherwise keep the same


--wat is KeyState: Up/Down?

{-}
--Als Asteroids én enemies op zijn
bulletChecker' [] ship (b:bs) [] | collision ship b = bulletChecker' [] (shipDeath(ship)) bs []                                                
                                 | otherwise = let (as', ship', bs', es') = (bulletChecker' [] ship bs []) in (as', ship', (b:bs'), es')

--Als alleen enemies op zijn stop dan met checken voor die collisions
bulletChecker' a ship (b:bs) [] | collision ship b = bulletChecker' a (shipDeath(ship)) bs []       
                                | collision a b = ((destroyAs(a)), ship, bs, [])        --OPLETTEN DAT DIT ALLEEN SHIP BULLETS ZIJN
                                | otherwise = let (as', ship', bs', es') = (bulletChecker' a ship bs []) in (as', ship', (b:bs'), es')


--Als alleen asteroids op zijn stop dan met checken voor die collisions
bulletChecker' [] ship (b:bs) e | collision ship b = bulletChecker' [] (shipDeath(ship)) bs e       
                                | collision e b = ([], ship, bs, [])
                                | otherwise = let (as', ship', bs', es') = (bulletChecker' [] ship bs e) in (as', ship', (b:bs'), es')
-}