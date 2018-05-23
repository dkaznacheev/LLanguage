import LLexer
import TokenParser
import Data.Char
import Control.Monad
import System.Environment

test1 :: ([LexToken], [LexToken])
test1 = ([LNum 1.0,Op Ominus,LNum 1.0], (runLex "1-1"))

test2 :: ([LexToken], [LexToken])
test2 = ([Sep Bracket_L,LNum 1.0,Sep Bracket_R,Op Oplus,LNum 1.0,LNum 2.0], (runLex " (       1) +   1       2    "))

test3 :: ([LexToken], [LexToken])
test3 = ([LNum 0.0,Op Oplus,LNum 13.0,Op Omul,LNum 42.0,Op Ominus,LNum 7.0,Op Odiv,LNum 0.0], (runLex "0 + 13 * 42 - 7 / 0"))

test4 :: (Program, Program)
test4 = (Program [] (Body [ComAs (Ident "x") (ExprOp Oplus (ExprId (Ident "x")) (ExprNum (PNum 1.0)))]), (buildTree "x := (x + 1);"))

test5 :: (Program, Program)
test5 = ((Program [FunDef (Ident "fun1") (ArgDef [Ident "x",Ident "y"]) (Body [ComAs (Ident "x") (ExprId (Ident "y"))]),FunDef (Ident "fun2") (ArgDef []) (Body [])] (Body [ComRead (Ident "x"),ComWrite (ExprOp Oplus (ExprId (Ident "x")) (ExprCall (FunCall (Ident "fun1") (Args [ExprCall (FunCall (Ident "fun2") (Args [])),ExprId (Ident "x")]))))])), (buildTree "fun1(x, y) {x := y;} fun2();read(x);write((x + fun1(fun2(), x)));"))

test6 :: (Program, Program)
test6 = ((buildTree "x += 1;"), (buildTree "x := (x + 1);"))

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
        runTest 4 test4
        runTest 5 test5
        runTest 6 test6