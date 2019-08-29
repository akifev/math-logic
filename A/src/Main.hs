module Main where
import Lexer
import Parser
import Grammar

main :: IO ()
main = do
    str <- getLine
    putStrLn (show (parser (alexScanTokens str)))
