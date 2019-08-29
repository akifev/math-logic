module Main where
import Lexer
import Parser
import Grammar

main :: IO ()
main = do
    str <- readFile "input.txt"
    let strs = map (parser . alexScanTokens) (lines str)
    writeFile "output.txt" ("[\n" ++ (unlines (map (\s -> (s ++ ",")) (map (show) strs))) ++ "]")