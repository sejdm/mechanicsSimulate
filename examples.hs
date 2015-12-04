import Mechanics
import Control.DeepSeq
import Graphics.Gloss

-- Solar System
data Solar = Sun | Earth | Moon deriving (Eq, Show)

instance Physical Solar where
        imass Sun   = 14000
        imass Earth = 10
        imass Moon  = 1
        
        iposition Sun = (-100, -100)
        iposition Earth = (0, 400)
        iposition Moon = (0, 430)

        ivelocity Sun = (0, 0)
        ivelocity Earth = (120, 0)
        ivelocity Moon = (70, 0)

        icolour Sun = yellow
        icolour Earth = light blue
        icolour Moon = greyN 0.3


solarSystemP = SystemP {
               objectsP = [Sun, Earth, Moon],
               internalP = gravitationPotential,
               externalP = noPotential
               }

solarSystem = System {
               objects = [Sun, Earth, Moon],
               internal = gravitationField,
               external = noField
               }

-- Centripetal
data Sling = Pivot | Ball deriving (Eq)

instance Physical Sling where
        ifixed Pivot = True
        ifixed _ = False

        istate Pivot = ((0, 0), (0, 0))
        istate Ball = ((100, 0), (0, 200))

        imass Pivot = 1
        imass Ball = 10

centripetalSystem = System {
                           objects = [Pivot, Ball],
                           internal = centripetal,
                           external = noField
                           }


--- Simple Harmonic
data Spring = Centre | Weight deriving (Eq)

instance Physical Spring where
        ifixed Centre = True
        ifixed _ = False

        iposition Centre = (0, 0)
        iposition Weight = (100, 0)

        ivelocity Centre = (0, 0)
        ivelocity Weight = (0, 0)

        imass Centre = 0
        imass Weight = 10



simpleHarmonicSystem = System {
                             objects = [Centre, Weight], 
                             internal = simpleHarmonic,
                             external = noField
                             }

-- Elastic Collision
data Balls = Ball1 | Ball2 deriving (Eq)

instance Physical Balls where
        iposition Ball1 = (-100, 0)
        iposition Ball2 = (500, 0)

        ivelocity Ball1 = (0, 0)
        ivelocity Ball2 = (-100, 0)

        imass Ball1 = 1
        imass Ball2 = 100
        
        ifixed Ball1 = False
        ifixed _ = False

collisionSystem = System {
                          objects = [Ball1, Ball2],
                          internal = noForce,
                          external = noField
                          }

main = simulateN solarSystem
--main = simulateN centripetalSystem

{-
numbered (n, s) = putStrLn $ show n ++ " " ++ s
main = do mapM_ numbered $ zip [0..] ["Solar", "Centripetal", "Simple Harmonic", "Collision"]
          ([
           simulateN solarSystem,
           simulateN centripetalSystem, 
           simulateN simpleHarmonicSystem, 
           simulateN collisionSystem
           ] !!) =<< readLn
-}
