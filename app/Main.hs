module Main (main) where

import Control.Monad.Except
import Data.List.Split
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random
import Text.Megaparsec
import Text.Megaparsec.CSV
import Text.Megaparsec.DIMACS.Graph

data Conf = Conf { cnfRandSeed :: Maybe Int, cnfEscapeChar :: Maybe Char, cnfSeperator :: Char, cnfUseDIMACS :: Bool, cnfNeedHelp :: Bool, cnfNumAttempts :: Int, cnfNois :: [String] } deriving(Eq)

defaultConf :: Conf
defaultConf = Conf { cnfRandSeed = Nothing, cnfEscapeChar = Nothing, cnfSeperator = ',', cnfUseDIMACS = False, cnfNeedHelp = False, cnfNumAttempts = 1024, cnfNois = []}

options :: [OptDescr (Conf -> Conf)]
options = [
    Option ['h'] ["help"] (NoArg (\cnf -> cnf { cnfNeedHelp = True})) "Print help message",
    Option ['S'] ["seed"] (ReqArg (\arg -> \cnf -> cnf { cnfRandSeed = (Just (read arg :: Int))}) "<integer>") "Specify what random seed to use. Default is to randomly pick a seed.",
    Option ['G'] ["dimacs"] (NoArg (cnf -> cnf {cnfUseDIMACES = True })) "Use DIMACS graph file format."
   ] 

setSeed :: Conf -> StdGen -> StdGen
setSeed (Conf { cnfRandSeed = Nothing}) seed = seed
setSeed (Conf { cnfRandSeed = (Just seed)}) _ = mkStdGen seed

main' :: String -> StdGen -> Conf -> Except String String
main' _ _ (Conf { cnfNeedHelp = True}) = return (usageInfo "Usage: tsalesman " options)

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

