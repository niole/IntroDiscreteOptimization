import System.Environment
import Data.Char
import Data.List (sortBy)

data Item = Item { index :: Int, value :: Double, weight :: Double }
        deriving (Show, Eq)
data Args = Args { items :: [Item], capacity :: Double }
        deriving (Show)
data Best = Best { k :: Double, v :: Double, bestGuess :: Double, chosen :: [Item] }
        deriving (Show, Eq)

instance Ord Best where
  compare a b = compare (v a) $ v b

getObjectiveFunc :: [Item] -> Double -> Double
getObjectiveFunc is capacity = fitElements is capacity $ fromIntegral 0
                               where fitElements (x:xs) k objFunc
                                        | k == 0 = objFunc
                                        | k > 0 && k - (weight x) >= 0 = fitElements xs (k - (weight x)) $ objFunc+ (value x)
                                        | k > 0 && k - (weight x) < 0 = fitElements xs 0 (objFunc + (value x) * (k / (weight x)))
                                     fitElements [] _ objFunc = objFunc
-- get available items differently or just do it all in getObjFunc
getAvailableItems :: [Item] -> [Item] -> [Item]
getAvailableItems is chosen = foldl rev is chosen
                                where rev cs c = c:cs

-- add or don't add current item
-- if recalculated objective function and estimate are both less than the current best, stop
-- calculate objective function with items that have already been chosen and those that have not yet been seen
-- that is the possible max value
solver :: [Item] -> Double -> Double -> Double -> Best -> [Item] -> Best
solver [] objFunc currCapacity currValue best chosen
        | currCapacity < 0 = best
        | objFunc <= (v best) = best
        | currCapacity >= 0 = max best $ Best currCapacity currValue objFunc chosen

solver (item:items) objFunc currCapacity currValue best chosen
        | currCapacity < 0 = best
        | objFunc <= (v best) = best
        | currCapacity == 0 = max best $ Best currCapacity currValue objFunc chosen
        | otherwise = let currBest = max best $ Best currCapacity currValue objFunc chosen
                          chosenWith = item:chosen
                          chosenWithout = chosen
                          bestWhenKept = solver items objFunc (currCapacity - (weight item)) (currValue + (value item)) currBest chosenWith
                          bestOverall = solver items (getObjectiveFunc (getAvailableItems items chosenWithout) currCapacity) currCapacity currValue bestWhenKept chosenWithout
                      in bestOverall

splitData :: String -> [String]
splitData as = words =<< lines as

createItems :: Int -> [String] -> [Item]
createItems _ [] = []
createItems index (v:w:rest) = (Item index (toDouble v) $ toDouble w):createItems (index+1) rest

toDouble :: String -> Double
toDouble n = read n :: Double

cleanInput :: [String] -> Args
cleanInput (_:capacity:items) = Args (createItems 0 items) $ toDouble capacity
cleanInput _ = Args (createItems 0 []) 0

sortByWeightDensity :: Double -> [Item] -> [Item]
sortByWeightDensity capacity is = snd <$> (sortBy sortDensity $ createWeights <$> is)
                                where sortDensity a b
                                       | fst a > fst b = LT
                                       | otherwise = GT
                                      createWeights i = ((value i) / (weight i), i)

formatOutput :: Best -> [Item] -> String
formatOutput best items = let cs = sortBy sortIndex $ chosen best
                          in (show $ v best) ++ " 1\n" ++ (unwords $ augment cs 0)
                          where sortIndex a b
                                  | index a < index b = LT
                                  | otherwise = GT
                                augment [] i = (\_ -> "0") <$> items
                                augment (c:chosen) i
                                        | i == (index c) = "1":augment chosen (i+1)
                                        | otherwise = "0":augment (c:chosen) (i+1) -- assume i <


main :: IO()
main = do
       args <- getArgs
       let a = args !! 0
           arg = cleanInput $ splitData a
           k = capacity arg
           is = sortByWeightDensity k $ items arg
           oF = getObjectiveFunc is k
       putStrLn $ show $ formatOutput (solver is oF k 0 (Best k 0 oF []) []) $ items arg

--data KnapsackProblem = KnapsackProblem { capacity :: Int, items :: [Item] }
--data Item = Item { weight :: Int, value :: Int }
--parseProblem :: String -> KnapsackProblem
--
--getObjectiveFunc :: [Item] -> Double -> Double
--
--getObjective :: SolverState -> Double
--getObjective ss = (getObjectiveFunc (remainingItems ss) (toDouble $ remainingCapacity ss))
--
--
--data SolverState = SolverState {
--  choices :: [Choice],
--  value :: Int,
--  remainingItems :: [Item],
--  remainingCapacity :: Int,
--  best :: [Choice]
--}
--
--solve :: KnapsackProblem -> SolverState
--solve kp = solve' $
--  SolverState {
--    choices=[],
--    value=0,
--    remainingItems=(items kp),
--    remainingCapacity=(capacity kp)
--  }
--
--data Choice = Choice { chosen :: Boolean, item :: Item }
--
--
--solve' :: SolverState -> SolverState
--solve' (SolverState choices value remainingItems remainingCapacity)
--        | remainingCapacity == 0 = -- base case: nextStates returns -- generate next two states using nextStates
---- recursively call solve on these two subproblems
---- need to thread the best argument through
