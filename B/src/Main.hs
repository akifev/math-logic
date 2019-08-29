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
loop []         context !implMap !allAsMap beenResult result !proof = Just (reverse proof)
loop (expr:xs) context !implMap !allAsMap beenResult result !proof =
                               let
                               exprAsLineProof = min (maybeModusPonens expr implMap allAsMap) (min (maybeAxiom expr) (maybeHypothesis context expr))
                               index = Map.size allAsMap
                                   in
                                       if isMyNothing exprAsLineProof then
                                           Nothing
                                       else
                                           let
                                           newAllAsMap = if isNothing (Map.lookup expr allAsMap) then (Map.insert expr index allAsMap) else allAsMap
                                           newProof = if beenResult == False && (Map.size allAsMap /= Map.size newAllAsMap) then (exprAsLineProof : proof) else proof
                                           been = beenResult || (expr == result)
                                               in
                                                   if isImpl expr then
                                                       let
                                                       l = getFrom expr
                                                       r = getTo expr
                                                       newImplMap = Map.insertWith (++) r [l] implMap
                                                           in
                                                               loop xs context newImplMap newAllAsMap been result newProof
                                                   else
                                                       loop xs context implMap newAllAsMap been result newProof



dfs [] !needed = needed
dfs proof@(line:xs) !needed = case line of
                    (ModusPonens l r e) -> let
                                          revProof = reverse proof
                                          l'th = revProof !! l
                                          r'th = revProof !! r
                                              in
                                                  if Set.member line needed then
                                                      dfs xs (Set.insert l'th (Set.insert r'th needed))
                                                  else
                                                      dfs xs needed
                    _ -> dfs xs needed

getAnswer [] _ !answer = reverse answer
getAnswer (line:xs) needed !answer = if Set.member line needed then
                                        getAnswer xs needed (line:answer)
                                    else
                                        getAnswer xs needed answer

updateIndexes byInd helper line = case line of
        (ModusPonens l r e) -> let
                              fstIndex = (fromJust (Map.lookup (fromJust (Map.lookup l byInd)) helper)) + 1
                              sndIndex = (fromJust (Map.lookup (fromJust (Map.lookup r byInd)) helper)) + 1
                                  in
                                      ModusPonens fstIndex sndIndex e
        a -> a

fixedIndexes [] needed !mapWithUpdatedIndexes = mapWithUpdatedIndexes
fixedIndexes (x:xs) needed !mapWithUpdatedIndexes = if Set.member x needed then
                                                       fixedIndexes xs needed (Map.insert x (Map.size mapWithUpdatedIndexes) mapWithUpdatedIndexes)
                                                   else
                                                       fixedIndexes xs needed mapWithUpdatedIndexes

-- Output
showProblem []      result = "|- " ++ (show result) ++ "\n"
showProblem [last] result = (show last) ++ " " ++ (showProblem [] result)
showProblem (x:xs) result = (show x) ++ ", " ++ (showProblem xs result)

-- Main
main = do
    firstLine  <- getLine
    let problem  = parseProblem $ alexScanTokens firstLine
    let context = (reverse $ getContext problem)
    let contextMap  = Map.fromList (zip context [1..])
    let result   = getResult problem
    proof'''    <- getContents
    let proof''   = map (parseExpression . alexScanTokens) (filter  (/=[]) (lines proof'''))
    if (last proof'') /= result then
        putStr "Proof is incorrect"
    else do
        let proof' = loop proof'' contextMap Map.empty Map.empty False result []
        if isNothing proof' then
            putStr "Proof is incorrect"
        else do
            let proof = fromJust proof'
            let byInd = Map.fromList (zip [0..] proof)
            let needed = dfs (reverse proof) (Set.singleton (last proof))
            let answer' = getAnswer proof needed []
            let helper = fixedIndexes proof needed Map.empty
            let answer = map (updateIndexes byInd helper) answer'
            putStr ((showProblem context result) ++ ((concat . (zipWith (\ind -> \t -> ("[" ++ (show ind) ++ ". " ++ (show t) ++ "\n")) [1..])) answer))