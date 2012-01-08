import Prelude hiding (lex)
import System.IO
import System.Random hiding (split)
import Control.Monad

import AST
import Evaluator
import Optimizer
import FileUtils
import MainCommon

-- Author: Aleksander Balicki
-- A binary for randomized testing, it randomizes the file, the environment and the starting state,
-- runs the optimizer and checks if the result is the same

main :: IO ()
main = do
    files <- listFiles "inputs"
    hSetBuffering stdout NoBuffering
    forever $ do
        file <- randomElem files
        cont <- readFile file

        randomOptimizationsWithNames <- filterM (const (randomIO :: IO Bool)) optimizations
        let randomOptimizations =  map fst randomOptimizationsWithNames
        let randomOptimizationNames = map snd randomOptimizationsWithNames

        ((oldAst, _), (newAst, _)) <- oldNewAstWithSize 10 (optimize' randomOptimizations) cont

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

