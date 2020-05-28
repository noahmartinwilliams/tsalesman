module Graph where

import Types
import Data.List.Split

getGraph :: String -> Char -> Graph
getGraph str separator = do
    let lines = endBy "\n" str
    let columns = map (\x -> endBy [separator] x) lines
    let edges = map (\[x, y, z] -> Edge x z (read y :: Double)) columns
    Graph edges
