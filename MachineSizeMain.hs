import Prelude hiding (lex)
import Lexer
import Parser
import MachineSize
import Control.Monad.State
import qualified Data.Map as Map

-- To run, pipe the machine into the binary

main = do
    cont <- getContents
    let (ast, _) = runState (parse . lex $ cont) (0, Map.empty)
    putStrLn $ "machine size: " ++ show (msize ast)
