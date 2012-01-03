import Prelude hiding (lex)
import System.Timeout
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent.MVar

import Lexer
import Parser
import Evaluator

main = do
    (timeArg:args) <- getArgs
    let time = read timeArg
    cont <- getContents
    bestSolution <- newMVar 5
    ast <- timeout (time*1000000) (swapMVar bestSolution 8 >> (return $ runMachine 0 (parse . lex $ cont) [("sbz_at_0", 0), ("opfmt_at_0", 1)] ))
    case ast of
        Nothing -> hPutStrLn stderr "timed out" >> exitFailure
        Just goodAst -> print goodAst >> (takeMVar bestSolution)
