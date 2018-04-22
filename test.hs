import LLexer
import Data.Char
import Control.Monad
import System.Environment


test1 :: ([LexToken], [LexToken])
test1 = ([LNum 1.0,Op Ominus,LNum 1.0], (runLex "1-1"))

test2 :: ([LexToken], [LexToken])
test2 = ([Sep Bracket_L,LNum 1.0,Sep Bracket_R,Op Oplus,LNum 1.0,LNum 2.0], (runLex " (       1) +   1       2    "))

test3 :: ([LexToken], [LexToken])
test3 = ([LNum 0.0,Op Oplus,LNum 13.0,Op Omul,LNum 42.0,Op Ominus,LNum 7.0,Op Odiv,LNum 0.0], (runLex "0 + 13 * 42 - 7 / 0"))


runTest :: (Show a, Eq a) => Int -> (a, a) -> IO ()
runTest n (ex, res) = do
    if (ex == res) then
        putStrLn $ "Test " ++ (show n) ++ " passed."
    else
        putStrLn $ "Test " ++ (show n) ++ " failed: expected " ++ (show ex) ++ ", got " ++ (show res) ++ "."

main :: IO ()
main = do
        runTest 1 test1
        runTest 2 test2
        runTest 3 test3