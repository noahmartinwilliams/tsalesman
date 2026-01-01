{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import Control.DeepSeq
import Control.Monad.Except
import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split
import Data.Map as Map
import GHC.Conc
import Graph.DijkstraSimple
import GHC.Generics
import Graph.TSalesman
import ParSort
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random
import Text.Megaparsec
import Text.Megaparsec.CSV
import Text.Megaparsec.DIMACS.Graph

instance NFData (EdgeTo v e) where
    rnf (EdgeTo { edgeTo = v, edgeToWeight = e})  = seq e (seq v ())

parseList :: String -> Conf -> [String]
parseList str cnf = do
    let parsed = runParser (csv (cnfSeperator cnf) (cnfEscapeChar cnf)) "argument" str
    case parsed of 
        (Left l) -> error (errorBundlePretty l)
        (Right r) -> r !! 0

data Conf = Conf { cnfRandSeed :: Maybe Int, cnfEscapeChar :: Maybe Char, cnfSeperator :: Char, cnfUseDIMACS :: Bool, cnfNeedHelp :: Bool, cnfNumAttempts :: Int, cnfNois :: [String] } deriving(Eq)

defaultConf :: Conf
defaultConf = Conf { cnfRandSeed = Nothing, cnfEscapeChar = Nothing, cnfSeperator = ',', cnfUseDIMACS = False, cnfNeedHelp = False, cnfNumAttempts = 1024, cnfNois = []}

options :: [OptDescr (Conf -> Conf)]
options = [
    Option ['h'] ["help"] (NoArg (\cnf -> cnf { cnfNeedHelp = True})) "Print help message",
    Option ['S'] ["seed"] (ReqArg (\arg -> \cnf -> cnf { cnfRandSeed = (Just (read arg :: Int))}) "<integer>") "Specify what random seed to use. Default is to randomly pick a seed.",
    Option ['G'] ["dimacs"] (NoArg (\cnf -> cnf {cnfUseDIMACS = True })) "Use DIMACS graph file format.",
    Option ['N'] ["noi"] (ReqArg (\arg -> \cnf -> cnf { cnfNois = (parseList arg cnf)}) "node1,node2,etc" ) "Specify nodes of interest in comma seperated list.",
    Option ['A'] ["attempts"] (ReqArg (\arg -> \cnf -> cnf {cnfNumAttempts = (read arg :: Int)} ) "<integer>") "Specify number of attempts to make.",
    Option ['s'] ["seperator"] (ReqArg (\arg -> \cnf -> cnf { cnfSeperator = (arg !! 0) }) "<char>") "Specify what seperator character to use for CSV files.",
    Option ['e'] ["escape"] (ReqArg (\arg -> \cnf -> cnf {cnfEscapeChar = (Just (arg !! 0))} ) "<char>" ) "Specify what the escape character should be for CSV files (default is no escape character)."
   ] 

setSeed :: Conf -> StdGen -> StdGen
setSeed (Conf { cnfRandSeed = Nothing}) seed = seed
setSeed (Conf { cnfRandSeed = (Just seed)}) _ = mkStdGen seed

parse :: String -> Conf -> Except String [(String, String, Double)]
parse str (Conf { cnfUseDIMACS = True}) = do
    let parsed = runParser grFile "stdin" str 
    case parsed of 
        (Left l) -> throwError (errorBundlePretty l)
        (Right r) -> let (GrFile _ arcs) = r in return (Prelude.map (\(ArcLine a b c) -> (show a, show b, fromIntegral c :: Double)) arcs)
parse str cnf = do
    let parsed = runParser (csv (cnfSeperator cnf) (cnfEscapeChar cnf)) "stdin" str 
    case parsed of 
        (Left l) -> throwError (errorBundlePretty l)
        (Right r) -> return (Prelude.map (\(a : b : c : _) -> (a, b, read c :: Double)) r)

main' :: String -> StdGen -> Conf -> Except String String
main' _ _ (Conf { cnfNeedHelp = True}) = return (usageInfo "Usage: tsalesman " options)
main' input stdgen cnf@(Conf { cnfRandSeed = rs, cnfNumAttempts = noA, cnfNois = nois}) = do
    parsed <- Main.parse input cnf
    let newSeed = setSeed cnf stdgen
        sorted = parSort parsed
        uniqed = deepseq sorted (uniqem sorted)
        uniqed' = (Prelude.map (\(a, b) -> (a, Prelude.map (\(c, d) -> EdgeTo { edgeTo = c, edgeToWeight = d}) b)) uniqed) `using` (parListChunk numCapabilities (rparWith rseq))
        mapped = deepseq uniqed' (Map.fromList uniqed')
        graph = Graph { graphAsMap = mapped }
        path = solveTSP graph newSeed (cnfNumAttempts cnf) (cnfNois cnf)
    return (Prelude.foldr (\x -> \y -> x ++ "\n" ++ y) "" path)

main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    g <- getStdGen
    hSetBuffering stdout (BlockBuffering Nothing)
    case getOpt Permute options args of
        (o, _, []) -> do
            let outp = runExcept (main' input g (Prelude.foldl (flip id) defaultConf o))
            case outp of
                (Left l) -> hPutStrLn stderr l
                (Right r) -> 
                    putStr r
        (_, _, errs) -> do
            hPutStrLn stderr (show errs)

