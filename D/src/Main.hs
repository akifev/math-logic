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
showProblem []     result = "|- " ++ (show result) ++ "\n"
showProblem [last] result = (show last) ++ " " ++ (showProblem [] result)
showProblem (x:xs) result = (show x) ++ ", " ++ (showProblem xs result)

findVariables (Var a) = Set.singleton a
findVariables (Not expr) = findVariables expr
findVariables expr    = findVariables (getFrom expr) `Set.union` findVariables (getTo expr)

mask cnt | cnt == 3  = [[False,False,False], [False,False,True], [False,True,False], [False,True,True], [True,False,False], [True,False,True], [True,True,False], [True,True,True]]
         | cnt == 2  = [[False,False]      , [False,True]      , [True,False]      , [True,True]]
         | cnt == 1  = [[False]            , [True]]

apply (Var a)    values = fromJust (Map.lookup a values)
apply (Not expr) values = not (apply expr values)
apply (Impl a b) values = not ((apply a values) == True && (apply b values) == False)
apply (Or a b)   values = not ((apply a values) == False && (apply b values) == False)
apply (And a b)  values = (apply a values) == True && (apply b values) == True

genProof (Var a)    values !proof = Just (a : proof)
genProof (Not expr) values !proof = Nothing
genProof (Impl a b) values !proof = if isJust (genImplProof a b values) then (fromJust (genImplProof a b values)) else Nothing
genProof (Or a b)   values !proof = if isJust (genOrProof a b values) then (fromJust (genOrProof a b values)) else Nothing
genProof (And a b)  values !proof = if isJust (genAndProof a b values) then (fromJust (genAndProof a b values)) else Nothing

genImplProof a b values = 

genOrProof a b values = if isJust (genProof a values []) then (fromJust a ) = 

-- Main
main = do
    result' <- getLine
    let result = parseExpression $ alexScanTokens result'
    putStrLn $ show result
    let variables = findVariables result
    let variablesAsList = Set.toList variables
    putStrLn $ show variables
    let n = Set.size variables
    let allValues = map (\t -> Map.fromList (zip variablesAsList t)) (mask n)
    putStrLn $ show allValues
    let applyed = filter (not . null) (map (\values -> if apply result values then values else Map.empty) allValues) -- list values with true evaluation
    putStrLn $ show applyed
    let proofs = 