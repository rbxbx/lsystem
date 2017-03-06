import Graphics.Gloss

-- algae
data AlgaeState = A | B
  deriving (Show)

algaeNext :: AlgaeState -> [AlgaeState]
algaeNext A = [A, B]
algaeNext B = [A]

algaeNextGeneration :: [AlgaeState] -> [AlgaeState]
algaeNextGeneration  = (>>= algaeNext)

algaeGenerations :: [[AlgaeState]]
algaeGenerations = iterate algaeNextGeneration [A]

-- koch curve
data KochState = F | Plus | Minus
  deriving (Show)

data KochBearing = U | D | L | R
  deriving (Show)

kochNext :: KochState -> [KochState]
kochNext F = [F, Plus, F, Minus, F, Minus, F, Plus, F]
kochNext x = [x]

kochNextGeneration :: [KochState] -> [KochState]
kochNextGeneration  = (>>= kochNext)

kochGenerations :: [[KochState]]
kochGenerations = iterate kochNextGeneration [F]

cw :: KochBearing -> KochBearing
cw U = R
cw D = L
cw R = D
cw L = U

ccw :: KochBearing -> KochBearing
ccw U = L
ccw D = R
ccw R = U
ccw L = D

delta :: Float
delta = 3

move :: KochBearing -> Point -> Point
move U (x, y) = (x, y + delta)
move D (x, y) = (x, y - delta)
move R (x, y) = (x + delta, y)
move L (x, y) = (x - delta, y)

kochPath :: [KochState] -> Path
kochPath = kp (0, 0) R
  where kp _ _ []         = []
        kp p b (F:xs)     = p' : kp p' b xs
          where p' = move b p
        kp p b (Plus:xs)  = kp p (ccw b) xs 
        kp p b (Minus:xs) = kp p (cw b) xs 

-- graphics
main :: IO ()
main = display window background drawing
  where background = white
        drawing = line . kochPath $ kochGenerations !! 5
        window = InWindow "Nice Window" (200, 200) (10, 10)
