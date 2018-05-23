import ArithParser
import LLexer
import Data.Char
import Control.Monad
import System.Environment

main :: IO ()
main = do
        [f] <- getArgs
        s <- readFile f
        let pr = buildTree s
        putStrLn $ printProgram pr