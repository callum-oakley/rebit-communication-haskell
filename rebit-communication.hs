import qualified Data.Map as Map
import Data.Fixed (mod')

type Message            = Integer
type Rebit              = Double
type Prob               = Double
type PartialCalculation = Map.Map Integer Prob

squareInnerProduct :: Rebit -> Rebit -> Prob
squareInnerProduct phi psi = (cos ((phi - psi) / 2)) ^ 2

divisions :: Message -> Integer -- now many slices to cut the nth cirle in to
divisions n = 2 ^ (div n 2 + 1)

edge :: Message -> Integer -> Rebit
edge n k = 2 * pi * fromInteger k / fromInteger (divisions n)

middle :: Message -> Integer -> Rebit
middle n k
    | circle n x == 1 = x
    | circle n y == 1= y
  where
    x = mod' ((edge n k + edge n (k + 1)) / 2) (2*pi)
    y = mod' ((edge n (k - 1) + edge n k) / 2) (2*pi)

circle :: Message -> Rebit -> Integer
circle m phi
    | odd m      = 1 - (circle (m - 1) phi') -- antisymmetry of cirle n and n+1
    | phi' >= pi = 1 - (circle m (phi' - pi)) -- antisymmetry of antipodal phi
    | even k     = 1
    | otherwise  = 0
  where
    phi' = mod' phi (2 * pi) -- normalise phi to the range [0, 2pi)
    k = floor ((fromInteger (divisions m) * phi) / (2 * pi))

calculateProb :: Rebit -> PartialCalculation -> Message -> PartialCalculation
calculateProb psi currentProbs n
    | circle n psi == 0 = Map.insert n 0 currentProbs -- Proposition 1
    | otherwise         = Map.insert n pMin currentProbs
  where
    pMin = minimum . map bound $ ks -- Proposition 3
    bound k = squareInnerProduct psi (edge n k) -
        sum [(currentProbs Map.! m) * fromInteger (circle m $ middle n k) |
        m <- [0..(n - 1)]]
    ks  | elem n [0, 1] = [0, 1]
        | otherwise =
            [1..(div (divisions n) 2 - 1)] ++
            [(div (divisions n) 2 + 1)..(divisions n - 1)] -- ignore vertical edges since they aren't boundaries after message 0 and 1

calculateProbs :: Rebit -> Integer -> Map.Map Integer Prob -- up to message n
calculateProbs phi n = foldl (calculateProb phi) Map.empty $ [0..n]
