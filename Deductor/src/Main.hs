module Main where
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lexer
import Parser
import Grammar

-- Hypothesis
maybeHypothesis context expression = case Map.lookup expression context of
                                      Nothing -> MyNothing expression
                                      Just a  -> Hypothesis a expression

isHypothesis context expression = (not . isMyNothing) (maybeHypothesis context expression)

-- Axiom
maybeAxiom expr@(Impl a (Impl b a'))                                         | a == a'                              = Axiom 1 expr
maybeAxiom expr@(Impl (Impl a b) (Impl (Impl a' (Impl b' c)) (Impl a'' c'))) | a == a' && a' == a'' && b == b' && c == c' = Axiom 2 expr
maybeAxiom expr@(Impl a (Impl b (And a' b')))                                | a == a' && b == b'                     = Axiom 3 expr
maybeAxiom expr@(Impl (And a b) a')                                          | a == a'                              = Axiom 4 expr
maybeAxiom expr@(Impl (And a b) b')                                          | b == b'                              = Axiom 5 expr
maybeAxiom expr@(Impl a (Or a' b))                                           | a == a'                              = Axiom 6 expr
maybeAxiom expr@(Impl b (Or a b'))                                           | b == b'                              = Axiom 7 expr
maybeAxiom expr@(Impl (Impl a c) (Impl (Impl b c') (Impl (Or a' b') c'')))   | a == a' && b == b' && c == c' && c' == c'' = Axiom 8 expr
maybeAxiom expr@(Impl (Impl a b) (Impl (Impl a' (Not b')) (Not a'')))        | a == a' && a' == a'' && b == b'          = Axiom 9 expr
maybeAxiom expr@(Impl (Not (Not a)) a')                                      | a == a'                              = Axiom 10 expr
maybeAxiom expr                                                                                                    = MyNothing expr

isAxiom expression = (not . isMyNothing) (maybeAxiom expression)

-- Modus Ponens
maybeModusPonens to !implMap !allAsMap =
                                    let
                                    fromList' = (Map.lookup to implMap)
                                    fromList = if isJust fromList' then fromJust fromList' else []
                                    left' = List.find (\l -> isJust (Map.lookup l allAsMap)) fromList
                                    leftIndex  = if isJust left' then fromJust (Map.lookup (fromJust left') allAsMap) else -1
                                        in
                                            if leftIndex /= -1 then
                                                let
                                                implIndex = fromJust (Map.lookup (Impl (fromJust left') to) allAsMap)
                                                    in
                                                        ModusPonens implIndex leftIndex to
                                            else
                                                MyNothing to

isModusPonens to !implMap !allAsMap = (not . isMyNothing) (maybeModusPonens to implMap allAsMap)

-- Check other conditions
isMyNothing smth = case smth of
     (MyNothing a) -> True
     _             -> False

isImpl expr = case expr of
     (Impl a b) -> True
     _          -> False

-- Build answer
loop []        contextMap !implMap !allAsMap !proof = reverse proof
loop (expr:xs) contextMap !implMap !allAsMap !proof =
                                let
                                exprAsLineProof = min (maybeModusPonens expr implMap allAsMap) (min (maybeAxiom expr) (maybeHypothesis contextMap expr))
                                index = Map.size allAsMap
                                newAllAsMap = if isNothing (Map.lookup expr allAsMap) then (Map.insert expr index allAsMap) else allAsMap
                                newProof = if (Map.size allAsMap /= Map.size newAllAsMap) then (exprAsLineProof : proof) else proof
                                newImplMap = if isImpl expr then (Map.insertWith (++) (getTo expr) [(getFrom expr)] implMap) else implMap
                                    in
                                        loop xs contextMap newImplMap newAllAsMap newProof

-- Output
showProblem []     result = (show result) ++ "\n"
showProblem [last] result = "|- " ++ (show last) ++ " -> " ++ (showProblem [] result)
showProblem (x:xs) result = (show x) ++ ", " ++ (showProblem xs result)

-- Deduction
func answer []     hyp = []
func answer (x:xs) hyp = case x of
                    (Axiom num expr) -> (expr : (Impl expr (Impl hyp expr)) : (Impl hyp expr) : (func answer xs hyp))
                    (Hypothesis num expr) -> if expr == hyp then
                                                 ((Impl expr (Impl expr expr)) : (Impl (Impl expr (Impl expr expr)) (Impl (Impl expr (Impl (Impl expr expr) expr)) (Impl expr expr))) : (Impl (Impl expr (Impl (Impl expr expr) expr)) (Impl expr expr)) : (Impl expr (Impl (Impl expr expr) expr)) : (Impl hyp expr) : []) ++ (func answer xs hyp)
                                             else
                                                 (expr : (Impl expr (Impl hyp expr)) : (Impl hyp expr) : []) ++ (func answer xs hyp)
                    (ModusPonens impl left expr) -> let
                                                    l = getExpression (answer !! (left - 1))
                                                        in
                                                            ((Impl (Impl hyp l) (Impl (Impl hyp (Impl l expr)) (Impl hyp expr))) : (Impl (Impl hyp (Impl l expr)) (Impl hyp expr)) : (Impl hyp expr) : []) ++ (func answer xs hyp)

-- Main
main = do
    input'  <- readFile "input.txt"
    let input = lines input'
    let firstLine = head input
    let problem  = parseProblem $ alexScanTokens firstLine
    let context = (reverse $ getContext problem)
    let contextMap  = Map.fromList (zip context [1..])
    let result   = getResult problem
    let proof''  = tail input
    let proof'   = map (parseExpression . alexScanTokens) (filter  (/=[]) proof'')
    let proof = loop proof' contextMap Map.empty Map.empty []
    let ded = func proof proof (last context)
    writeFile "output.txt" ((showProblem context result) ++ (unlines (map show ded)))
            