import System.Environment
import Data.Char
import Data.List (sortBy)

data Item = Item { index :: Int, value :: Double, weight :: Double }
        deriving (Show, Eq)
data Args = Args { items :: [Item], capacity :: Double }
        deriving (Show)
data Best = Best { k :: Double, v :: Double, bestGuess :: Double, chosen :: [Item] }
        deriving (Show, Eq)
data KnapsackState = KnapsackState { currItems :: [Item], objFunc :: Double, currCapacity :: Double,  currValue :: Double, best :: Best, selected :: [Item] }

class BranchNBoundSolver a where
  isSuboptimalRoute :: a -> Bool
  done :: a -> Bool
  getObjectiveFunction :: a -> Double
  considerItem :: a -> a
  ignoreItem :: a -> Best -> a

instance BranchNBoundSolver KnapsackState where
  isSuboptimalRoute s = objFunc s <= (v $ best s) || currCapacity s < 0
  done = f
         where f (KnapsackState [] _ _ _ _ _) = True
               f (KnapsackState _ _ capacity _ _ _) = capacity == 0
  considerItem (KnapsackState (i:is) oF currCapacity currValue best selected) = KnapsackState is oF (currCapacity - (weight i)) (currValue + (value i)) (max best $ Best currCapacity currValue oF selected) (i:selected)
  getObjectiveFunction ks = let (_:is) = currItems ks
                                chosen = selected ks
                                k = currCapacity ks
                            in fitElements (getAvailableItems is chosen) k $ fromIntegral 0
                                where getAvailableItems is chosen = foldl rev is chosen
                                                                        where rev cs c = c:cs
                                      fitElements (x:xs) k objFunc
                                        | k == 0 = objFunc
                                        | k > 0 && k - (weight x) >= 0 = fitElements xs (k - (weight x)) $ objFunc+ (value x)
                                        | k > 0 && k - (weight x) < 0 = fitElements xs 0 (objFunc + (value x) * (k / (weight x)))
                                      fitElements [] _ objFunc = objFunc

  ignoreItem ks bestWhenKept = let k = currCapacity ks
                                   v = currValue ks
                                   chosen = selected ks
                                   (_:is) = currItems ks
                               in KnapsackState is (getObjectiveFunction ks) k v bestWhenKept chosen

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
solver :: KnapsackState -> Best
solver ks
  | isSuboptimalRoute ks = best ks
  | done ks = max (best ks) $ Best (currCapacity ks) (currValue ks) (objFunc ks) (selected ks)
  | otherwise = let bestWhenKept = solver $ considerItem ks
              in solver $ ignoreItem ks bestWhenKept

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
                          in unwords $ augment cs items
                          where sortIndex a b
                                  | index a < index b = LT
                                  | otherwise = GT
                                augment [] items = (\_ -> "0") <$> items
                                augment (c:chosen) (item:items)
                                        | (index item) == (index c) = "1":augment chosen items
                                        | otherwise = "0":augment (c:chosen) items

main :: IO()
main = do
       args <- getArgs
       let a = args !! 0
           arg = cleanInput $ splitData a
           k = capacity arg
           is = sortByWeightDensity k $ items arg
           oF = getObjectiveFunc is k
           best = solver $ KnapsackState is oF k 0 (Best k 0 oF []) []
       putStrLn $ (show $ round $ v best) ++ " 1"
       putStrLn $ show $ formatOutput best $ items arg
