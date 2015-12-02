import qualified Data.Map as Map

type Message            = Integer
type Rebit              = Float
type Prob               = Float
type PartialCalculation = (Map.Map Integer Prob, [Rebit])

squareInnerProduct :: Rebit -> Rebit -> Prob
squareInnerProduct phi psi = (cos ((phi - psi) / 2)) ^ 2

divisions :: Message -> Integer -- now many slices to cut the mth cirle in to
divisions m = 2 ^ (div m 2 + 1)

boundaryRebits :: Message -> [Rebit]
boundaryRebits m = [2 * pi * fromInteger k / fromInteger d | k <- [0..(d - 1)]]
  where
    d = divisions m

isBlack :: Message -> Rebit -> Integer -- 0 for no, 1 for yes
isBlack m phi
    | odd m     = 1 - (isBlack (m - 1) phi) -- antisymmetry of cirle n and n+1
    | phi > pi  = 1 - (isBlack m (phi - pi)) -- antisymmetry of antipodal phi
    | odd k     = 0
    | otherwise = 1
  where
    k = floor ((fromInteger (divisions m) * phi) / (2 * pi))

calculateProb :: Rebit -> Message -> PartialCalculation -> PartialCalculation
calculateProb psi m (currentProbs, extremalRebits)
    | isTriviallyZero = (Map.insert m 0 currentProbs, extremalRebits)
    | otherwise       = (Map.insert m pMin currentProbs, phiMin:extremalRebits)
  where
    hasOverlap = or [isBlack m phi == 1 | phi <- extremalRebits]
    isTriviallyZero = isBlack m psi == 0 || hasOverlap
    extractMin phi (pMin, phiMin)
        | p < pMin = (p, phi)
        | otherwise = (pMin, phiMin)
      where
        p = squareInnerProduct psi phi
    (pMin, phiMin) = foldr extractMin (1, 0) (boundaryRebits m)

calculateProbs :: Rebit -> Integer -> Map.Map Integer Prob -- up to message n
calculateProbs phi n = fst . foldr (calculateProb phi) (Map.empty, []) $ [0..n]
