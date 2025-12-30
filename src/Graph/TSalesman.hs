module TSalesman(randomizeOrder) where

import Data.List
import Graph.DijkstraSimple
import System.Random

randomizeOrder :: (Ord a) => StdGen -> [a] -> [a]
randomizeOrder g list = do
    let ints = randoms g :: [Int]
        zipped = Prelude.zip ints list
        sorted = sort zipped
        (_, newL) = Prelude.unzip sorted
    newL
