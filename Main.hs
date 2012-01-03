import Prelude hiding (lex)
import System.Timeout
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent.MVar

import Lexer
import Parser

main = do
    (timeArg:args) <- getArgs
    let time = read timeArg
    cont <- getContents
    bestSolution <- newMVar 5
    ast <- timeout (time*1000000) ((swapMVar bestSolution 8)>>(return $ parse . lex $ cont))
    case ast of
        Nothing -> hPutStrLn stderr "timed out" >> exitFailure
        Just goodAst -> print goodAst >> (takeMVar bestSolution)
