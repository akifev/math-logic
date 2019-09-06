module Main where
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lexer
import Parser
import Grammar
import System.Exit (exitSuccess)

-- Hypothesis
maybeHypothesis context expression = case Map.lookup expression context of
                                      Nothing -> MyNothing expression
                                      Just a  -> Hypothesis a expression

isHypothesis context expression = (not . isMyNothing) (maybeHypothesis context expression)

-- Axiom
maybeAxiom expr@(Impl a (Impl b a'))                                         | a == a'                                    = Axiom 1 expr
maybeAxiom expr@(Impl (Impl a b) (Impl (Impl a' (Impl b' c)) (Impl a'' c'))) | a == a' && a' == a'' && b == b' && c == c' = Axiom 2 expr
maybeAxiom expr@(Impl a (Impl b (And a' b')))                                | a == a' && b == b'                         = Axiom 3 expr
maybeAxiom expr@(Impl (And a b) a')                                          | a == a'                                    = Axiom 4 expr
maybeAxiom expr@(Impl (And a b) b')                                          | b == b'                                    = Axiom 5 expr
maybeAxiom expr@(Impl a (Or a' b))                                           | a == a'                                    = Axiom 6 expr
maybeAxiom expr@(Impl b (Or a b'))                                           | b == b'                                    = Axiom 7 expr
maybeAxiom expr@(Impl (Impl a c) (Impl (Impl b c') (Impl (Or a' b') c'')))   | a == a' && b == b' && c == c' && c' == c'' = Axiom 8 expr
maybeAxiom expr@(Impl (Impl a b) (Impl (Impl a' (Not b')) (Not a'')))        | a == a' && a' == a'' && b == b'            = Axiom 9 expr
maybeAxiom expr@(Impl (Not (Not a)) a')                                      | a == a'                                    = Axiom 10 expr
maybeAxiom expr                                                                                                           = MyNothing expr

isAxiom expression = (not . isMyNothing) (maybeAxiom expression)

-- Modus Ponens
maybeModusPonens to !implMap !allAsMap =
    let
    fromList' = (Map.lookup to implMap)
    fromList = if isJust fromList' then fromJust fromList' else []
    left' = List.find (\l -> isJust (Map.lookup l allAsMap)) fromList
        in
            if isJust left' then
                ModusPonens (Impl (fromJust left') to) (fromJust left') to
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

-- Output
showProblem []     result = "|- " ++ (show result) ++ "\n"
showProblem [last] result = (show last) ++ " " ++ (showProblem [] result)
showProblem (x:xs) result = (show x) ++ ", " ++ (showProblem xs result)

-- Deduction output
showDeductionProblem []            result = (show result) ++ "\n"
showDeductionProblem [last]        result = "|- " ++ (show last) ++ " -> " ++ (showDeductionProblem [] result)
showDeductionProblem [plast, last] result = (show plast) ++ " |- " ++ (show last) ++ " -> " ++ (showDeductionProblem [] result)
showDeductionProblem (x:xs)        result = (show x) ++ ", " ++ (showDeductionProblem xs result)

-- Deduction
deduction context lastHyp []        !implMap !allAsMap = []
deduction context lastHyp (expr:xs) !implMap !allAsMap = 
    let
    exprAsLineProof = min (maybeModusPonens expr implMap allAsMap) (min (maybeAxiom expr) (maybeHypothesis context expr))                    
    index = Map.size allAsMap
    newAllAsMap = if isNothing (Map.lookup expr allAsMap) then (Map.insert expr index allAsMap) else allAsMap
    newImplMap = if isImpl expr && (Map.size allAsMap /= Map.size newAllAsMap) then (Map.insertWith (++) (getTo expr) [(getFrom expr)] implMap) else implMap 
        in
            case exprAsLineProof of    
                    (Axiom num expr) -> 
                                        (
                                        expr 
                                        :(Impl expr (Impl lastHyp expr))
                                        :(Impl lastHyp expr)
                                        :(deduction context lastHyp xs newImplMap newAllAsMap)
                                        )
                    (Hypothesis num expr) -> if expr == lastHyp then
                                                    (
                                                    (Impl expr (Impl expr expr))
                                                    :(Impl (Impl expr (Impl expr expr)) (Impl (Impl expr (Impl (Impl expr expr) expr)) (Impl expr expr)))
                                                    :(Impl (Impl expr (Impl (Impl expr expr) expr)) (Impl expr expr))
                                                    :(Impl expr (Impl (Impl expr expr) expr))
                                                    :(Impl lastHyp expr)
                                                    :(deduction context lastHyp xs newImplMap newAllAsMap)
                                                    ) 
                                             else
                                                    (
                                                    expr
                                                    :(Impl expr (Impl lastHyp expr))
                                                    :(Impl lastHyp expr)
                                                    :(deduction context lastHyp xs newImplMap newAllAsMap)
                                                    )
                    (ModusPonens implExpr l expr) ->
                                                    (
                                                    (Impl (Impl lastHyp l) (Impl (Impl lastHyp (Impl l expr)) (Impl lastHyp expr)))
                                                    :(Impl (Impl lastHyp (Impl l expr)) (Impl lastHyp expr))
                                                    :(Impl lastHyp expr)
                                                    :(deduction context lastHyp xs newImplMap newAllAsMap)
                                                    )

-- Task D
findVariables (Var a) = Set.singleton a
findVariables (Not expr) = findVariables expr
findVariables expr    = findVariables (getFrom expr) `Set.union` findVariables (getTo expr)

genHyp [] = [[]]
genHyp (x:xs) = [x : t | t <- genHyp xs] ++ [(Not x) : t | t <- genHyp xs]

apply term@(Var a) context = if isJust $ List.find (==term) context then True else False
apply (Not expr) context = not $ apply expr context
apply (Impl a b) context = not $ apply a context && (not $ apply b context)
apply (Or a b) context = apply a context || apply b context 
apply (And a b) context = apply a context && apply b context

invert (Not expr) = expr
invert expr = Not expr

minimizeContext []     context' !list = list
minimizeContext (x':xs) context' !list =
    let
    context = getContext context'
    x = getContext x'
    newContext = List.intersect x context
    diff       = (List.\\) x newContext
        in
            if (((length diff) == 1) 
                && (((isJust (List.find (== (head diff)) x)) 
                    && (isJust (List.find (== (invert (head diff))) context)))
                || ((isJust (List.find (== (invert (head diff))) x)) 
                    && (isJust (List.find (== (head diff)) context)))))
            then
                minimizeContext xs context' ((ComplexContext newContext context' x') : list)
            else
                minimizeContext xs context' list

minimizeContexts []     !llist = llist
minimizeContexts (x':xs) !llist = minimizeContexts xs ((minimizeContext xs x' []) ++ llist)

isCorrect []           = True
isCorrect ((Not a):xs) = False  
isCorrect (x:xs)       = isCorrect xs

genSmartProof (SimpleContext context) result = genSimpleProof result context
genSmartProof (ComplexContext context smartFirstContext smartSecondContext) result =
               deduction (Map.fromList (zip context [1..])) hyp (genSmartProof smartFirstContext result) Map.empty Map.empty
            ++ deduction (Map.fromList (zip context [1..])) (invert hyp) (genSmartProof smartSecondContext result) Map.empty Map.empty
            ++ aNotA hyp
            ++ [
                (Impl (Impl hyp result) (Impl (Impl (Not hyp) result) (Impl (Or hyp (Not hyp)) result))),
                (Impl (Impl (Not hyp) result) (Impl (Or hyp (Not hyp)) result)),
                (Impl (Or hyp (Not hyp)) result),
                result
                ]
                                where
                                hyp = head ((List.\\) context (getContext smartFirstContext))

-- Algorithm    
worker result' variablesExpr flag = do
    
    let checkCorrect    | flag      = isCorrect
                        | otherwise = not . isCorrect

    let result          | flag      = result'
                        | otherwise = (Not result') 

    let hyp3 = map SimpleContext (List.filter (apply result) (genHyp variablesExpr))
    let hyp2 = minimizeContexts hyp3 []
    let hyp1 = minimizeContexts hyp2 []
    let hyp0 = minimizeContexts hyp1 []

    let context3 = List.filter (checkCorrect . getContext) hyp3
    let context2 = List.filter (checkCorrect . getContext) hyp2
    let context1 = List.filter (checkCorrect . getContext) hyp1
    let context0 = List.filter (checkCorrect . getContext) hyp0

    putStrLn $ unlines $ map show context3
    putStrLn $ unlines $ map show context2
    putStrLn $ unlines $ map show context1
    putStrLn $ unlines $ map show context0
    putStrLn ""

    -- putStrLn $ unlines $ map show (deduction (Map.fromList (zip [] [1..])) lastHyp (genSmartProof smartFirstContext result) Map.empty Map.empty)
    
    let answer  | (not . null ) context0 = do
                                            putStrLn $ unlines $ map show (genSmartProof (head context0) result)
                                            exitSuccess
                | (not . null ) context1 = do
                                            putStrLn $ unlines $ map show (genSmartProof (head context1) result)
                                            exitSuccess
                | (not . null ) context2 = do
                                            putStrLn $ unlines $ map show (genSmartProof (head context2) result)
                                            exitSuccess
                | (not . null ) context3 = do
                                            putStrLn $ unlines $ map show (genSmartProof (head context3) result)
                                            exitSuccess
                | otherwise = putStrLn ""
    answer
    
    --exitSuccess

-- Main
main = do
    result' <- getLine
    let result = parseExpression $ alexScanTokens result'
    
    let variablesSet = findVariables result              
    let variablesList = Set.toList variablesSet          
    let variablesExpr = map (\s -> (Var s)) variablesList

    worker result variablesExpr True
    worker result variablesExpr False
    putStrLn ":("


genSimpleProof (And a b) context = genSimpleProof a context
                                ++ genSimpleProof b context
                                ++ [
                                    (Impl a (Impl b (And a b))),
                                    (Impl b (And a b)),
                                    (And a b)
                                    ]

genSimpleProof (Or a b) context | apply a context = 
                                                    genSimpleProof a context
                                                ++ [
                                                    (Impl a (Or a b)),
                                                    (Or a b)
                                                    ]
                                | otherwise       = 
                                                    genSimpleProof b context
                                                ++ [
                                                    (Impl b (Or a b)),
                                                    (Or a b)
                                                    ]

genSimpleProof (Impl a b) context | apply b context = 
                                                    genSimpleProof b context
                                                ++ [
                                                    (Impl b (Impl a b)),
                                                    (Impl a b)
                                                    ]
                                  | _       =
                                                genSimpleProof (Not a) context
                                                ++ genSimpleProof (Not b) context
                                                ++ [
                                                    (Impl (Not a) (Impl a (Not a))),
                                                    (Impl a (Not a)),
                                                    (Impl (Not b) (Impl a (Not b))),
                                                    (Impl a (Not b)),
                                                    (Impl a (Impl a a)),
                                                    (Impl a (Impl (Impl a a) a)),
                                                    (Impl (Impl a (Impl a a)) (Impl (Impl a (Impl (Impl a a) a)) (Impl a a))),
                                                    (Impl (Impl a (Impl (Impl a a) a)) (Impl a a)),
                                                    (Impl a a),
                                                    (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b)))),
                                                    (Impl (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b)))) (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b)))))),
                                                    (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b))))),
                                                    (Impl a (Impl (Not b) a)),
                                                    (Impl (Impl a (Impl (Not b) a)) (Impl a (Impl a (Impl (Not b) a)))),
                                                    (Impl a (Impl a (Impl (Not b) a))),
                                                    (Impl (Impl a a) (Impl (Impl a (Impl a (Impl (Not b) a))) (Impl a (Impl (Not b) a)))),
                                                    (Impl (Impl a (Impl a (Impl (Not b) a))) (Impl a (Impl (Not b) a))),
                                                    (Impl a (Impl (Not b) a)),
                                                    (Impl (Not a) (Impl (Not b) (Not a))),
                                                    (Impl (Impl (Not a) (Impl (Not b) (Not a))) (Impl a (Impl (Not a) (Impl (Not b) (Not a))))),
                                                    (Impl a (Impl (Not a) (Impl (Not b) (Not a)))),
                                                    (Impl (Impl a (Not a)) (Impl (Impl a (Impl (Not a) (Impl (Not b) (Not a)))) (Impl a (Impl (Not b) (Not a))))),
                                                    (Impl (Impl a (Impl (Not a) (Impl (Not b) (Not a)))) (Impl a (Impl (Not b) (Not a)))),
                                                    (Impl a (Impl (Not b) (Not a))),
                                                    (Impl (Impl a (Impl (Not b) a)) (Impl (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b))))) (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))))),
                                                    (Impl (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b))))) (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b))))),
                                                    (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))),
                                                    (Impl (Impl a (Impl (Not b) (Not a))) (Impl (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))) (Impl a (Not (Not b))))),
                                                    (Impl (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))) (Impl a (Not (Not b)))),
                                                    (Impl a (Not (Not b))),
                                                    (Impl (Not (Not b)) b),
                                                    (Impl (Impl (Not (Not b)) b) (Impl a (Impl (Not (Not b)) b))),
                                                    (Impl a (Impl (Not (Not b)) b)),
                                                    (Impl (Impl a (Not (Not b))) (Impl (Impl a (Impl (Not (Not b)) b)) (Impl a b))),
                                                    (Impl (Impl a (Impl (Not (Not b)) b)) (Impl a b)),
                                                    (Impl a b)
                                                    ]
                                        
genSimpleProof (Not (And a b)) context  | apply a context = 
                                                genSimpleProof (Not b) context
                                            ++ [
                                                (Impl (And a b) b),
                                                (Impl (Not b) (Impl (And a b) (Not b))),
                                                (Impl (And a b) (Not b)),
                                                (Impl (Impl (And a b) b) (Impl (Impl (And a b) (Not b)) (Not (And a b)))),
                                                (Impl (Impl (And a b) (Not b)) (Not (And a b))),
                                                (Not (And a b))
                                                ]
                                        | otherwise      =
                                                genSimpleProof (Not a) context
                                            ++ [
                                                (Impl (And a b) a),
                                                (Impl (Not a) (Impl (And a b) (Not a))),
                                                (Impl (And a b) (Not a)),
                                                (Impl (Impl (And a b) a) (Impl (Impl (And a b) (Not a)) (Not (And a b)))),
                                                (Impl (Impl (And a b) (Not a)) (Not (And a b))),
                                                (Not (And a b))
                                                ]

genSimpleProof (Not (Or a b)) context = [
                                        (Impl (Impl (Not a) (Impl (Not b) (Not a))) (Impl a (Impl (Not a) (Impl (Not b) (Not a))))),
                                        (Impl (Not a) (Impl (Not b) (Not a))),
                                        (Impl a (Impl (Not a) (Impl (Not b) (Not a))))
                                        ]
                                    ++ genSimpleProof (Not a) context
                                    ++ [
                                        (Impl (Not a) (Impl a (Not a))),
                                        (Impl a (Not a)),
                                        (Impl (Impl a (Not a)) (Impl (Impl a (Impl (Not a) (Impl (Not b) (Not a)))) (Impl a (Impl (Not b) (Not a))))),
                                        (Impl (Impl a (Impl (Not a) (Impl (Not b) (Not a)))) (Impl a (Impl (Not b) (Not a)))),
                                        (Impl a (Impl (Not b) (Not a))),
                                        (Impl a (Impl (Not b) a)),
                                        (Impl (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b)))) (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b)))))),
                                        (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b)))),
                                        (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b))))),
                                        (Impl (Impl a (Impl (Not b) a)) (Impl (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b))))) (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))))),
                                        (Impl (Impl a (Impl (Impl (Not b) a) (Impl (Impl (Not b) (Not a)) (Not (Not b))))) (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b))))),
                                        (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))),
                                        (Impl (Impl a (Impl (Not b) (Not a))) (Impl (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))) (Impl a (Not (Not b))))),
                                        (Impl (Impl a (Impl (Impl (Not b) (Not a)) (Not (Not b)))) (Impl a (Not (Not b)))),
                                        (Impl a (Not (Not b))),
                                        (Impl (Impl (Not (Not b)) b) (Impl a (Impl (Not (Not b)) b))),
                                        (Impl (Not (Not b)) b),
                                        (Impl a (Impl (Not (Not b)) b)),
                                        (Impl (Impl a (Not (Not b))) (Impl (Impl a (Impl (Not (Not b)) b)) (Impl a b))),
                                        (Impl (Impl a (Impl (Not (Not b)) b)) (Impl a b)),
                                        (Impl a b),
                                        (Impl b (Impl (Impl b b) b)),
                                        (Impl b (Impl b b)),
                                        (Impl (Impl b (Impl b b)) (Impl (Impl b (Impl (Impl b b) b)) (Impl b b))),
                                        (Impl (Impl b (Impl (Impl b b) b)) (Impl b b)),
                                        (Impl b b),
                                        (Impl (Impl a b) (Impl (Impl b b) (Impl (Or a b) b))),
                                        (Impl (Impl b b) (Impl (Or a b) b)),
                                        (Impl (Or a b) b)
                                        ]
                                    ++  genSimpleProof (Not b) context
                                    ++ [
                                        (Impl (Not b) (Impl (Or a b) (Not b))),
                                        (Impl (Or a b) (Not b)),
                                        (Impl (Impl (Or a b) b) (Impl (Impl (Or a b) (Not b)) (Not (Or a b)))),
                                        (Impl (Impl (Or a b) (Not b)) (Not (Or a b))),
                                        (Not (Or a b))
                                        ]

genSimpleProof (Not (Impl a b)) context = 
                                       genSimpleProof a context
                                    ++ genSimpleProof (Not b) context
                                    ++ [
                                        (Impl (Not b) (Impl (Impl a b) (Not b))),
                                        (Impl (Impl a b) (Not b)),
                                        (Impl a (Impl (Impl a b) a)),
                                        (Impl (Impl a b) a),
                                        (Impl (Impl a b) (Impl (Impl a b) (Impl a b))),
                                        (Impl (Impl (Impl a b) (Impl (Impl a b) (Impl a b))) (Impl (Impl (Impl a b) (Impl (Impl (Impl a b) (Impl a b)) (Impl a b))) (Impl (Impl a b) (Impl a b)))),
                                        (Impl (Impl (Impl a b) (Impl (Impl (Impl a b) (Impl a b)) (Impl a b))) (Impl (Impl a b) (Impl a b))),
                                        (Impl (Impl a b) (Impl (Impl (Impl a b) (Impl a b)) (Impl a b))),
                                        (Impl (Impl a b) (Impl a b)),
                                        (Impl (Impl (Impl a b) a) (Impl (Impl (Impl a b) (Impl a b)) (Impl (Impl a b) b))),
                                        (Impl (Impl (Impl a b) (Impl a b)) (Impl (Impl a b) b)),
                                        (Impl (Impl a b) b),
                                        (Impl (Impl (Impl a b) b) (Impl (Impl (Impl a b) (Not b)) (Not (Impl a b)))),
                                        (Impl (Impl (Impl a b) (Not b)) (Not (Impl a b))),
                                        (Not (Impl a b))
                                        ]

genSimpleProof (Not (Not a)) context = 
                        genSimpleProof a context
                    ++ [
                        (Impl a (Impl (Not (Not (Not a))) a)),
                        (Impl (Not (Not (Not a))) a),
                        (Impl (Not (Not (Not a))) (Not a)),
                        (Impl (Impl (Not (Not (Not a))) a) (Impl (Impl (Not (Not (Not a))) (Not a)) (Not (Not (Not (Not a)))))),
                        (Impl (Impl (Not (Not (Not a))) (Not a)) (Not (Not (Not (Not a))))),
                        (Not (Not (Not (Not a)))),
                        (Impl (Not (Not (Not (Not a)))) (Not (Not a))),
                        (Not (Not a))
                        ]
            
genSimpleProof a context = [a]

aNotA expression = [
                    (Impl expression (Or expression (Not expression))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))),
                    (Impl (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl expression (Or expression (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl expression (Or expression (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl expression (Or expression (Not expression))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))),
                    (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))),
                    (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression)))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression)))))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl expression (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression))))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression))))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression)))) (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression))))),
                    (Impl (Impl (Impl expression (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl expression (Not (Or expression (Not expression)))) (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))) (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression)))),
                    (Impl (Impl expression (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not expression))),
                    (Impl (Not (Or expression (Not expression))) (Not expression)),
                    (Impl (Not expression) (Or expression (Not expression))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))),
                    (Impl (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Not expression) (Or expression (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Not expression) (Or expression (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Not expression) (Or expression (Not expression))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))),
                    (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))),
                    (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression)))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Not (Or expression (Not expression))))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Not (Or expression (Not expression)))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))),
                    (Impl (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression)))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression)))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Not expression) (Not (Or expression (Not expression))))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression)))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression)))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression))))) (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Not expression)))))),
                    (Impl (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Impl (Not (Or expression (Not expression))) (Impl (Impl (Not expression) (Not (Or expression (Not expression)))) (Not (Not expression)))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))) (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Not expression))))),
                    (Impl (Impl (Not expression) (Or expression (Not expression))) (Impl (Not (Or expression (Not expression))) (Not (Not expression)))),
                    (Impl (Not (Or expression (Not expression))) (Not (Not expression))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Not expression)) (Impl (Impl (Not (Or expression (Not expression))) (Not (Not expression))) (Not (Not (Or expression (Not expression)))))),
                    (Impl (Impl (Not (Or expression (Not expression))) (Not (Not expression))) (Not (Not (Or expression (Not expression))))),
                    (Not (Not (Or expression (Not expression)))),
                    (Impl (Not (Not (Or expression (Not expression)))) (Or expression (Not expression))),
                    (Or expression (Not expression))
                    ]