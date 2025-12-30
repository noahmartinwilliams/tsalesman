module Graph.TSalesman(solveTSP) where

import Control.DeepSeq
import Control.Monad.Trans.Select
import Control.Parallel
import Control.Parallel.Strategies
import Data.Function.Memoize
import Data.List
import Data.Maybe
import GHC.Conc
import Graph.DijkstraSimple
import Graph.DijkstraSimple.Weighters
import Numeric.IEEE
import System.Random

randomizeOrder :: (Ord a) => StdGen -> [a] -> [a]
randomizeOrder g list = do
    let ints = randoms g :: [Int]
        zipped = Prelude.zip ints list
        sorted = sort zipped
        (_, newL) = Prelude.unzip sorted
    newL

grade :: (NFData a, Ord a, NFData r, Ord r) => [[a]] -> ([a] -> r) -> [a]
grade [] _ = []
grade list k = do
    let zipped = deepseq list (Prelude.zip (Prelude.map k list `using` parListChunk numCapabilities (rdeepseq)) list )
        (_, (r:_)) = deepseq zipped (Prelude.unzip (sort zipped))
    r
    
gradeOrders :: (NFData a, Ord a, NFData r, Ord r) => [[a]] -> Select r [a]
gradeOrders list = select $ \k -> grade list k

getOrderings :: Ord a => StdGen -> Int -> [a] -> [[a]]
getOrderings randSeed numAttempts nois = do
    let randInts = randoms randSeed :: [Int]
        randSeeds = Prelude.map mkStdGen randInts
        randOrderings = Prelude.map (\x -> randomizeOrder x nois) randSeeds
    Prelude.take numAttempts randOrderings

bestTSP :: (Ord a, NFData a) => StdGen -> Int -> [a] -> Select Double [a]
bestTSP randSeed numAttempts list = do
    let orderings = getOrderings randSeed numAttempts list
    graded <- gradeOrders orderings
    return graded

dijkstraD :: Ord v => Graph v Double -> [v] -> Double
dijkstraD _ [] = 0.0
dijkstraD _ [_] = 0.0
dijkstraD graph [a, b] = do
    let maybePath = findPath graph a cumulativeWeighter b
    if isJust maybePath
    then 
        let (Just (Path { pathWeight = w})) = maybePath in w
    else
        infinity

dijkstraD graph (head : head' : rest) = do
    let d = dijkstraD graph [head, head']
    d + (dijkstraD graph (head' : rest))

solveTSP :: (Ord v, Memoizable v, NFData v) => Graph v Double -> StdGen -> Int -> [v] -> [v]
solveTSP graph randSeed numAttempts nois = do
    runSelect (bestTSP randSeed numAttempts nois) (memoize $ dijkstraD graph)
