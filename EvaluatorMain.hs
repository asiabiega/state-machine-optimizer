import Prelude hiding (lex)
import System.Environment
import Control.Monad.State

import Parser
import Lexer
import Evaluator
import AST

main :: IO ()
main = do
    cont <- getContents
    let (ast, state) = runState (parse . lex $ cont) tagStart
    args <- getArgs
    case args of
        (_:_:_) -> error "too many args supplied, use one Integer to set the machine state and pipe the machine to stdin"
        [] -> error "no args supplied, use one Integer to set the machine state and pipe the machine to stdin"
        [n] -> do
            renv <- randomEnv ast
            putStrLn $ "env: " ++ show renv
            let (t, c) = runMachine (read n) ast renv
            print t
            putStrLn $ "cost " ++ show c
