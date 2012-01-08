import Prelude hiding (lex)
import System.Timeout
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent.MVar
import System.Random hiding (split)
import Data.List
import Control.Monad

import Lexer
import Parser
import AST
import Evaluator
import MachineSize
import Optimizer
import Optimizer2
import FileUtils
import MainCommon

main = do
    files <- listFiles "inputs"
    hSetBuffering stdout NoBuffering
    forever $ do
        file <- randomElem files
        cont <- readFile file

        (randomOptimizationsWithNames) <- filterM (const (randomIO :: IO Bool)) optimizations
        let randomOptimizations = foldl (>>=) return $ map fst randomOptimizationsWithNames
        let randomOptimizationNames = map snd randomOptimizationsWithNames

        let ((oldAst, _), (newAst, _)) = oldNewAstWithSize randomOptimizations cont

        renv <- randomEnv oldAst

        randomStartingState <- randomElem (startingStates oldAst)

        let (ot, oc) = runMachine randomStartingState oldAst renv
        let (nt, nc) = runMachine randomStartingState newAst renv
        if ot /= nt
            then do
                putStrLn ""
                putStrLn $ "file:" ++ show file
                putStrLn $ "env:" ++ show renv
                putStrLn $ "starting state:" ++ show randomStartingState
                putStrLn $ "optimizations" ++ show randomOptimizationNames
                putStrLn $ "difference:" ++ show (ot, oc) ++ " " ++ show (nt, nc)
            else putStr "."

randomIndex :: [a] -> IO Int
randomIndex l = randomRIO (0, length l - 1)

randomElem :: [a] -> IO a
randomElem l = do
    ridx <- randomIndex l
    return $ l !! ridx
