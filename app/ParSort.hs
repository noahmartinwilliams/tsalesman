module ParSort where

import GHC.Conc
import Data.List

filter2 :: (a -> Bool) -> [a] -> ([a], [a])
filter2 _ [] = ([], [])
filter2 fn (head : tail) | fn head = let (tail1, tail2) = filter2 fn tail in (head : tail1, tail2)
filter2 fn (head : tail) = let (tail1, tail2) = filter2 fn tail in (tail1, head : tail2)

parSort :: (Ord a) => [a] -> [a]
parSort [] = []
parSort [a, b] | a >= b = [b, a]
parSort [a, b] = [a, b]
parSort (pivot : tail) = do
    let (ge, lt) = filter2 (\x -> x >= pivot) tail
    par ge (ge ++ (pivot : lt))

uniqem :: (Eq a) => [(a, a, b)] -> [(a, [(a, b)])]
uniqem [] = []
uniqem ((a, b, c) : tail) = intern (a, [(b, c)]) tail where
    intern :: (Eq a) => (a, [(a, b)]) -> [(a, a, b)] -> [(a, [(a, b)])]
    intern (a, b) [] = [(a, b)]
    intern (a, b) ((a', b', c') : tail) | a == a' = intern (a, (b', c') : b) tail
    intern a ((a', b, c) : tail) = a : (intern (a', [(b, c)]) tail)
