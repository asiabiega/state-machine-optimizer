import Prelude hiding (lex)
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

optimize :: Character -> Character
optimize = sameArgBranchRemoval . notAccessibleBranchRemoval . stateNumberWildcarder . contradictoryAndRemoval 

main = do
    (timeArg:args) <- getArgs
    let time = read timeArg
    cont <- getContents
    bestSolution <- newMVar 5
    let oldAst = parse . lex $ cont
    ast <- timeout (time*1000000) (swapMVar bestSolution 8 >> (return . optimize $ oldAst))
    case ast of
        Nothing -> hPutStrLn stderr "timed out" 
        Just goodAst -> do
            putStrLn $ "old size: " ++ show ( msize oldAst) --(takeMVar bestSolution) --TODO print new ast
            putStrLn $ "new size: " ++ show (msize goodAst)
            putStrLn $ "delta:" ++ show (msize oldAst - msize goodAst)
