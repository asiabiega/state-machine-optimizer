import Prelude hiding (lex)
import System.Timeout
import System.Directory
import System.Environment
import System.IO
import System.Exit
import System.FilePath.Posix
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

allOptimizations = optimizations ++ optimizations2

-- | A function inspired by python's string.split().  A list is split
-- on a separator which is itself a list (not a single element).
split :: Eq a => [a] -> [a] -> [[a]]
split tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ [] = Nothing
          sp1 t s = case find (t `isSuffixOf`) $ inits s of
                      Nothing -> Just (s, [])
                      Just p -> Just (take (length p - length t) p,
                                      drop (length p) s)

-- | Lists all filenames in a given directory
listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    allfiles <- getDirectoryContents path
    let files = sort $ filter (\s -> last (split "/" s) `notElem` [".", ".."]) allfiles
    return $ map (path </>) files

main = do
    files <- listFiles "inputs"
    forever $ do
        file <- randomElem files 
        
        cont <- readFile file
        let oldAst = parse . lex $ cont

        (randomOptimizationsWithNames) <- filterM (const (randomIO :: IO Bool)) allOptimizations
        let randomOptimizations = foldl (.) id $ map fst randomOptimizationsWithNames
        let randomOptimizationNames = map snd randomOptimizationsWithNames

        renv <- randomEnv oldAst

        randomStartingState <- randomElem (startingStates oldAst)

        ast <- timeout (10000000) (return . randomOptimizations $ oldAst)
        case ast of
            Nothing -> hPutStrLn stderr "timed out" 
            Just newAst -> do
                let (ot, oc) = runMachine randomStartingState oldAst renv
                let (nt, nc) = runMachine randomStartingState newAst renv
                if ot /= nt
                    then do
                        putStrLn $ "file:" ++ (show file)
                        putStrLn $ "env:" ++ (show renv)
                        putStrLn $ "starting state:" ++ (show randomStartingState)
                        putStrLn $ "optimizations" ++ (show randomOptimizationNames)
                        putStrLn $ "difference:" ++ (show (ot, oc)) ++ " " ++ (show (nt, nc))
                    else putStrLn "."

randomIndex :: [a] -> IO Int
randomIndex l = randomRIO (0, length l - 1)

randomElem :: [a] -> IO a
randomElem l = do
    ridx <- randomIndex l
    return $ l !! ridx
