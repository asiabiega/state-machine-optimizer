import Prelude hiding (lex)
import Lexer
import Parser
main = getContents >>= print . parse . lex
