module MainCommon where
-- Author: Aleksander Balicki
-- Some common functions for the binaries, controller for the optimizations and size computation
import Prelude hiding (lex)
import Control.Monad.State
import System.Timeout

import MachineSize
import Lexer
import Parser
import AST

oldNewAstWithSize :: Int ->  (Character -> Tagger Character) -> String -> IO ((Character, Integer), (Character, Integer))
oldNewAstWithSize time opt cont = do
    let (oldAst, state) = runState (parse . lex $ cont) tagStart
    let oldSize = msize oldAst
    ast <- timeout (time*1000000) (return $ runState (opt $ oldAst) state)
    case ast of
        Just (newAst, _) -> return ((oldAst, oldSize), (newAst, msize newAst))
        Nothing -> return ((oldAst, oldSize), (oldAst, oldSize))

oldAstWithSize :: String -> (Character, Integer)
oldAstWithSize cont = let (oldAst, _) = runState (parse . lex $ cont) tagStart in
    let oldSize = msize oldAst in
    (oldAst, oldSize)

