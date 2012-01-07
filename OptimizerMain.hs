import Prelude hiding (lex)
import Data.Ratio
import System.Timeout
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent.MVar

import Lexer
import Parser
import AST
import MachineSize
import Optimizer
import Optimizer2
import FileUtils

optimize :: Character -> Character
optimize = sameArgBranchRemoval . notAccessibleBranchRemoval . stateNumberWildcarder . contradictoryAndRemoval . trivialAndRemoval

sizePairContent :: String -> IO (Integer, Integer)
sizePairContent cont = do
    let oldAst = parse . lex $ cont
    let oldSize = msize oldAst
    ast <- return . optimize $ oldAst
    return (oldSize, msize ast)

main = do
    [arg] <- getArgs
    case arg of
        "-a" -> do
            files <- listFiles "inputs/"
            contentlist <- mapM readFile files
            sizelist <- mapM (\c -> sizePairContent c) contentlist
            let (oldSizeSum, newSizeSum) = foldl (\(x,y) (x2, y2) -> (x+x2, y+y2)) (0,0) sizelist
            putStrLn $ "old size sum: " ++ show oldSizeSum
            putStrLn $ "new size sum: " ++ show newSizeSum
            putStrLn $ "sum delta: " ++ show (oldSizeSum - newSizeSum) ++ " (" ++
                 show (100.0 - (fromIntegral newSizeSum / fromIntegral oldSizeSum * 100.0)) ++ "% reduction)"

        _ -> do
            let time = read arg
            cont <- getContents
--            bestSolution <- newMVar 5
            let oldAst = parse . lex $ cont
            ast <- timeout (time*1000000) (return . optimize $ oldAst)
            case ast of
                Nothing -> hPutStrLn stderr "timed out"
                Just goodAst -> do
                    putStrLn $ "old size: " ++ show ( msize oldAst) --(takeMVar bestSolution) --TODO print new ast
                    putStrLn $ "new size: " ++ show (msize goodAst)
                    putStrLn $ "delta:" ++ show (msize oldAst - msize goodAst)
