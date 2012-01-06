import Prelude hiding (lex)
import Lexer
import Parser
import MachineSize

main = do
    cont <- getContents
    let ast = parse . lex $ cont
    putStrLn $ "machine size: " ++ (show $ msize ast)
