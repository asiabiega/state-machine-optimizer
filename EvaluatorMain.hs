import Prelude hiding (lex)
import System.Environment
import System.Random

import Parser
import Lexer
import Evaluator
import AST


main :: IO ()
main = do
    char <- fmap (parse . lex) getContents
    args <- getArgs
    case args of
        (_:_:_) -> error "too many args supplied, use one Integer to set the machine state"
        [] -> error "no args supplied, use one Integer to set the machine state"
        [n] -> do 
            renv <- randomEnv char
            putStrLn $ "env: " ++ show renv
            let (t, c) = runMachine (read n) char renv
            print t
            putStrLn $ "cost " ++ show c
