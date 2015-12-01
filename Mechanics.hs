module Mechanics (RealNumber, Time, Vector2D, Position, Velocity, Acceleration, Force, Momentum, Mass, Charge, State, Field, noField, noPotential,
                 Body (..), simulateAll,
                 Newtonian (..),
                 System (..), simulateN, 
                 SystemP (..), simulateNP, 
                 massToRadius,
                 transformAll, solve, setValues,
                 combineFields,  combineEffects, potentialToField, combinePotentials, combinePotentialEffects,
                 gravitationPotential, gravitationField, gravity, centripetal, simpleHarmonic, pendulum, falling, noForce, potentialEnergy, 
                 (#+), (#-), (#*), (#/), neg, dot, norm, dist, arg, orthArg, dir, orthDir, proj, grad) where

import Graphics.Gloss
import Control.DeepSeq
import Data.List
import Graphics.Gloss.Data.ViewPort (ViewPort)



-- If useEnhanced = False then the exactly stepsPerFrame steps will be
-- performed per second and each step will be displayed as a frame. Use
-- useEnhanced = False if you do not mind the number of frames being very
-- large if you want a high accuracy..
stepsPerSecond :: Int
stepsPerSecond = 1000 -- Start with a small number first. For  my computer 12500 works fine but if the number is too high, it will be rendered jerky.

useEnhanced :: Bool
useEnhanced = False -- Setting this False usually results in a smoother one but then the frames per second are high.

-- If useEnhanced = True, then exactly framesPerSecond frames will be
-- displayed, but for each frame stepsPerFrame steps will be performed. Use
-- useEnhanced = True  if you want the number of frames to be fixed but the
-- accuracy to increase. 
framesPerSecond :: Int -- Used only if useEnhanced = True
framesPerSecond = 50

stepsPerFrame :: Int -- Used only if useEnhanced = True
stepsPerFrame =  1140






type RealNumber = Float
type Time = RealNumber
type Vector2D = (RealNumber, RealNumber)
type Position = (RealNumber, RealNumber)
type Velocity = (RealNumber, RealNumber)
type Acceleration = (RealNumber, RealNumber)
type Force = (RealNumber, RealNumber)
type Momentum = (RealNumber, RealNumber)
type Mass = RealNumber
type Charge = RealNumber
type State = (Position, Velocity)
type Field = State -> Acceleration
type ExternalField = Field
type Potential = Position -> RealNumber
type ExternalPotential = Potential

data Body a = Body {
        object :: a,
        mass :: Mass,
        charge :: Mass,
        radius :: RealNumber,
        fixed :: Bool,
        position :: Position,
        velocity :: Velocity,
        toPicture :: Picture
        } deriving (Eq, Show)


instance NFData (Body a) where
        rnf (Body o m c r f p v t) = o `seq` m `seq` c `seq` r `seq` f `seq` p `deepseq` v `deepseq` t `seq` Body o m c r f p v t `seq` ()


updateState' :: Body a -> State -> Body a
updateState' x s | fixed x   = x 
                 | otherwise = updateState x s

updateState :: Body a -> State -> Body a
updateState x (p, v) = x {position = p, velocity = v}

getState :: Body a -> State
getState x = (position x, velocity x)



positionPicture :: Body a  -> Picture
positionPicture o = translate x1 y1 i
                where (x1, y1) = position o
                      i = toPicture o

positionPictures :: [Body a] -> Picture
positionPictures = pictures . map positionPicture






(#+) :: Vector2D -> Vector2D -> Vector2D
(x1, y1) #+ (x2, y2) = (x1+ x2, y1+y2)

(#-) :: Vector2D -> Vector2D -> Vector2D
(x1, y1) #- (x2, y2) = (x1- x2, y1-y2)

(#*) :: RealNumber -> Vector2D -> Vector2D
c #* (x, y) = (c*x, c*y) 

(#/) :: Vector2D -> RealNumber -> Vector2D
v #/ c = (1.0 / c) #* v

neg :: Vector2D -> Vector2D
neg v = (-1) #* v

dot :: Vector2D -> Vector2D -> RealNumber
(x1, y1) `dot` (x2, y2) = x1*x2 + y1*y2

norm :: Vector2D -> RealNumber
norm v = sqrt $ v `dot` v

arg :: Vector2D -> Vector2D
arg v = v #/ norm v

orthArg :: Vector2D -> Vector2D
orthArg (x, y) = arg (-y, x)

dist :: Point -> Point -> RealNumber
dist p1 p2 = norm (p2 #- p1)

dir :: Point -> Point -> Vector2D
dir v w = arg (w #- v)

orthDir :: Point -> Point -> Vector2D
orthDir v w = orthArg (w #- v)

proj :: Vector2D -> Vector2D -> Vector2D
proj v w = (v `dot` e) #* e where e = arg w

grad :: (Vector2D -> RealNumber) -> Vector2D -> Vector2D
grad f (x, y) = ((f (x + h, y) - f (x, y))/h, (f (x, y + h) - f (x, y))/h)
    where h = 0.0001




collision :: Body a -> Body a -> Body a
collision o2 o1 
    | radius o1 + radius o2 >= dist p1 p2 = updateState o2 $ if fixed o1 && radius o1 /= 0 then (p2, neg vh2 #+ vv2) 
                                                       else (p2, ((((m2 - m1) #* vh2) #+ ((2 * m1) #* vh1)) #/ (m1 + m2)) #+ vv2)
    | otherwise = o2
    where vh1 = velocity o1 `proj` dir p1 p2
          vh2 = velocity o2 `proj` dir p1 p2
          vv2 = velocity o2 `proj` orthDir p1 p2
          p1 = position o1
          p2 = position o2
          m1 = mass o1
          m2 = mass o2

collisions ::  Eq a => [Body a] -> Body a -> Body a
collisions os o = foldl' collision o  $ filter (/=o) os






applyField :: Time -> Field -> State -> State
applyField h f s@(p1, v1) = (p1 + h #* v1, v1 + h #* a)
    where a  = f s

combineFields :: [Field] -> Field
combineFields fs s = foldl1' (\x y -> force (x #+ y)) [ f s | f <- fs]

combineEffects :: [Body a -> Field] -> Body a -> Field
combineEffects fs x = combineFields [f x | f <- fs]

fieldExcludingOne :: Eq a => (Body a -> Field) -> [Body a] -> Body a -> Field
fieldExcludingOne f os x = combineFields [f o | o <- os, o /= x]



potentialToField :: Potential -> Field
potentialToField f (p, _) = grad f  p

combinePotentials :: [Potential] -> Potential
combinePotentials fs x = sum [f x | f <- fs]

combinePotentialEffects :: [Body a -> Potential] -> Body a -> Potential
combinePotentialEffects fs x = combinePotentials [f x | f <- fs]

transformAll ::  Eq a => (Body a -> Field) -> ExternalField  -> Time -> [Body a] -> [Body a]
transformAll f g t os =   [updateState' x $ applyField t (combineFields [fieldExcludingOne f os x, g]) (getState $ collisions os x) | x <- os]

glossify :: (Time -> b -> b) -> ViewPort -> Time -> b -> b
glossify f _ = f

simulateAllStep ::  Eq a => (Body a -> Field) -> ExternalField -> ViewPort -> Time -> [Body a] -> [Body a]
simulateAllStep f g = glossify (transformAll f g)

simulateAll :: Eq a =>  [Body a] -> (Body a -> Field) -> ExternalField -> IO ()
simulateAll os f g = standardSimulate os positionPictures (simulateAllStep f g)

standardSimulate :: NFData a =>  a -> (a -> Picture) -> (ViewPort -> Float -> a -> a) -> IO ()
standardSimulate = if useEnhanced then standardSimulateEnhanced else standardSimulateNormal

standardSimulateNormal :: a -> (a -> Picture) -> (ViewPort -> Float -> a -> a) -> IO ()
standardSimulateNormal = simulate (InWindow "Mechanics" (600, 600) (20, 20)) white stepsPerSecond


-- This block is only for an enhanced simulation
standardSimulateEnhanced :: NFData a =>  a -> (a -> Picture) -> (ViewPort -> Float -> a -> a) -> IO ()
standardSimulateEnhanced x f g = standardSimulate' x f (enhance stepsPerFrame g)
    where standardSimulate' = simulate (InWindow "Mechanics" (600, 600) (20, 20)) white framesPerSecond

last' :: NFData a => [a] -> a
last' [x] = x
last' (x:xs) = x `deepseq` last xs

enhance :: NFData a => Int -> (ViewPort -> Float -> a -> a) -> ViewPort -> Float -> a -> a
enhance n f v t i = last' $ take n $ iterate (f v (t/fromIntegral n)) i
-- End of endhanced simulation block





massToRadius :: Mass -> RealNumber
massToRadius m = sqrt 10 * sqrt m


radiusToMass :: RealNumber -> Mass
radiusToMass r = r^(2 :: Int)/10




class Newtonian a where
        imass :: a -> Mass
        imass = radiusToMass . iradius

        iradius :: a -> RealNumber
        iradius = massToRadius . imass

        icharge :: a -> Charge
        icharge _ = 0

        iposition :: a -> Position
        iposition = fst . istate

        ivelocity :: a -> Velocity
        ivelocity = snd . istate

        istate :: a -> State
        istate x = (iposition x, ivelocity x)

        ifixed :: a -> Bool
        ifixed _ = False

        icolour :: a -> Color
        icolour _ = black

        ipicture :: a -> Picture
        ipicture x = color (icolour x) $ circleSolid (iradius x)


setValues :: Newtonian a => a -> Body a
setValues x = Body {
                    object = x,
                    mass = imass x, 
                    radius = iradius x, 
                    charge = icharge x, 
                    position = iposition x, 
                    velocity = ivelocity x, 
                    fixed = ifixed x, 
                    toPicture = ipicture x
                    }


simulateNewton :: (Eq a, Newtonian a) => [a] -> (Body a -> Field) -> ExternalField -> IO ()
simulateNewton xs = simulateAll (map setValues xs)


simulateNewtonPotential :: (Eq a, Newtonian a) => [a] -> (Body a -> Potential) -> ExternalPotential -> IO ()
simulateNewtonPotential xs f g = simulateNewton xs (potentialToField . f) (potentialToField g)








data System a = System { objects :: [a], internal :: Body a -> Field , external :: ExternalField }

simulateN :: (Newtonian a, Eq a) => System a -> IO ()
simulateN x = simulateNewton (objects x) (internal x) (external x) 


data SystemP a = SystemP { objectsP :: [a], internalP :: Body a -> Potential , externalP :: ExternalPotential }

simulateNP :: (Newtonian a, Eq a) => SystemP a -> IO ()
simulateNP x = simulateNewtonPotential  (objectsP x) (internalP x) (externalP x) 





gConst :: RealNumber
gConst = 8000

gravitationPotential :: Body a -> Potential
gravitationPotential o1 p2 = gConst * m1 / dist p1 p2
        where m1 = mass o1
              p1 = position o1

gravitationField :: Body a -> Field
gravitationField o1 (p2, _) = ((gConst * m1) / (dist p1 p2 ^(2 :: Int)) ) #* dir p2 p1
        where m1 = mass o1
              p1 = position o1

centripetal :: Body a -> Field
centripetal o1 (p2, v2) = (((v `dot` v) / dist p1 p2) #* dir p2 p1 ) #- vn
                where p1 = position o1
                      v = (v2 #- velocity o1) `proj` orthDir p2 p1
                      vn = (v2 #- velocity o1) `proj` dir p1 p2


simpleHarmonic :: Body a -> Field
simpleHarmonic o1 (p2, _)  = ((-10) * dist p1 p2) #* dir p1 p2
        where p1 = position o1

noForce :: Body a -> Field
noForce _ _ = (0, 0)

falling :: Field
falling _ = (0, -8)

potentialEnergy :: Potential
potentialEnergy p = -8 * norm p

pendulum :: Body a -> Field
pendulum o1 (p2, _) = fl #+ proj fl (dir p1 p2)
    where p1 = position o1
          fl = (0, -20)


gravity :: Body a -> Body a -> Force
gravity o1 o2 = mass o2 #* gravitationField o1 (getState o2)


noField :: Field
noField _ = (0, 0)

noPotential :: Potential
noPotential _ = 0

solve :: NFData a => RealNumber -> RealNumber -> a -> (RealNumber -> a -> a) -> RealNumber -> a
solve h it iy f t = helper it iy
    where helper t' y' | t' >= t = y'
                       | otherwise = t' `seq` y' `deepseq` helper (t' + h) (f h y')

instance Functor Body where
        fmap f x = x {object = f (object x)}

instance Monad Body where
        return x = Body {object = x, mass = 0, charge = 0, radius = 0, position = (0, 00), velocity = (0, 0), fixed = False, toPicture = Blank }
        (>>=) = undefined
