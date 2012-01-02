import Prelude hiding (lex)
import Lexer

main = getContents >>= print . lex
