module MainCommon where
import Prelude hiding (lex)
import Control.Monad.State

import MachineSize
import Lexer
import Parser
import AST

oldNewAstWithSize :: (Character -> Tagger Character) -> String -> IO ((Character, Integer), (Character, Integer))
oldNewAstWithSize opt cont = do
    let (oldAst, state) = runState (parse . lex $ cont) tagStart
    let oldSize = msize oldAst
    let (newAst, _) = runState (opt oldAst) state
    let newSize = msize newAst
    return ((oldAst, oldSize), (newAst, newSize))

oldAstWithSize :: String -> (Character, Integer)
oldAstWithSize cont = let (oldAst, _) = runState (parse . lex $ cont) tagStart in
    let oldSize = msize oldAst in
    (oldAst, oldSize)

