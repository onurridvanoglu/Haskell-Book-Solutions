import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die = 
      DieOne 
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq,Show)

intToDie :: Int -> Die
intToDie n = 
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, _) = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

-- Exercise 
-- 1
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
                | sum >= n = count
                | otherwise =
                    let (die, nextGen) = randomR (1, 6) gen
                    in go (sum + die) (count + 1) nextGen

-- 2
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = (length dice, dice)
  where dice = map intToDie (rolls 0 0 g)
        rolls :: Int -> Int -> StdGen -> [Int]
        rolls dsum count gen
          | dsum >= n   = []
          | otherwise   = let (die, nextGen) = randomR (1, 6) gen
                           in die : (rolls (dsum + die) (count + 1) nextGen)

rollsCountLogged' :: Int -> StdGen -> (Int, [Die])
rollsCountLogged' n g = (length shit, shit)
    where shit = map intToDie (goshit 0 0 g)
          goshit :: Int -> Int -> StdGen -> [Int]
          goshit sum count gen
            | sum >= n = []
            | otherwise = 
                let (die, nextGen) = randomR (1, 6) gen
                in die : (goshit (sum + die) (count + 1) nextGen)