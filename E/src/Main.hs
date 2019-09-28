module Main where
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lexer
import Parser
import Grammar

-- Hypothesis
maybeHypothesis contextMap expression = case Map.lookup expression contextMap of
    Nothing -> MyNothing expression
    Just a  -> Hypothesis a expression

isHypothesis contextMap expression = (not . isMyNothing) (maybeHypothesis contextMap expression)

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

-- Check other conditions
isMyNothing smth = case smth of
    (MyNothing a) -> True
    _             -> False

isImpl expr = case expr of
    (Impl a b) -> True
    _          -> False

------------------------------Task E------------------------------------------------------------------------

-- типа в сете будет лежать Nothing, если где-то лажа
incorrectSubstitute = Set.singleton (Nothing)

-- возвращает сет термов для замены, если рядом с квантором не x и всё корректно. 
checkUndexQuantifier x v v' w w' a b
    | x /= v && v == v' = checkSubstitute x w w' 
    | x == v && a == b  = Set.empty
    | otherwise         = incorrectSubstitute


{-
возвращает сет термов, которые были типа заменены вместо свободных вхождений икса
-}
checkTerms x []                     []                        = Set.empty
checkTerms x []                     b                         = incorrectSubstitute
checkTerms x a                      []                        = incorrectSubstitute
checkTerms x ((Plus from to):terms) ((Plus from' to'):terms') = Set.union (Set.union (checkTerms x [from] [from']) (checkTerms x [to] [to'])) (checkTerms x terms terms')
checkTerms x ((Mul  from to):terms) ((Mul  from' to'):terms') = Set.union (Set.union (checkTerms x [from] [from']) (checkTerms x [to] [to'])) (checkTerms x terms terms')
checkTerms x ((Inc  t)      :terms) ((Inc  t')       :terms') = Set.union (checkTerms x [t] [t']) (checkTerms x terms terms')
checkTerms x ((Func f ts)   :terms) ((Func f' ts')   :terms') = if f == f' then Set.union (checkTerms x ts ts') (checkTerms x terms terms') else incorrectSubstitute
checkTerms x ((Zero)        :terms) ((Zero)          :terms') = checkTerms x terms terms'
checkTerms x (a@(Var name)  :terms) (maybeNewTerm    :terms') = if x == name then Set.union (Set.singleton (Just maybeNewTerm)) (checkTerms x terms terms') else if a == maybeNewTerm then checkTerms x terms terms' else incorrectSubstitute
checkTerms x a                      b                         = incorrectSubstitute 


{-
сначала нужно проверить бета на корректность, для этого нужно
проверить, что все свободные вхождения икс в альфа заменены на одно и то же

checkSubstitute возвращает сет термов для замены.
инвариант, что если сет содержит значение Nothing, то бета некорректно получено
инвариант, что если сет размера 1, (и там не Nothing), то бета корретно получено
-}
checkSubstitute x (Impl from to) (Impl from' to') = Set.union (checkSubstitute x from from') (checkSubstitute x to to')
checkSubstitute x (Or from to)   (Or from' to')   = Set.union (checkSubstitute x from from') (checkSubstitute x to to')
checkSubstitute x (And from to)  (And from' to')  = Set.union (checkSubstitute x from from') (checkSubstitute x to to')
checkSubstitute x (Not e)        (Not e')         = checkSubstitute x e e'
checkSubstitute x a@(ForAll v w) b@(ForAll v' w') = checkUndexQuantifier x v v' w w' a b
checkSubstitute x a@(Exist v w)  b@(Exist v' w')  = checkUndexQuantifier x v v' w w' a b
checkSubstitute x (Pred p terms) (Pred p' terms') = if p == p' then checkTerms x terms terms' else incorrectSubstitute
checkSubstitute x (PVar p)       (PVar p')        = if p == p' then Set.empty else incorrectSubstitute
checkSubstitute x (Equal l r)    (Equal l' r')    = Set.union (checkTerms x [l] [l']) (checkTerms x [r] [r'])
checkSubstitute x a              b                = incorrectSubstitute


{-
checkCorrectSubstitute - принимает сетик термов для подстановки из функции checkSubstitute, 
                         проверяет инвариант корректности
-}
checkCorrectSubstitute terms = if Set.size terms == 1 && Set.notMember (Nothing) terms then True else False


{-
getTermForSubstitute - если checkCorrectSubstitute, возвращает тот самый терм (без проверки). 
                        Останется только проверить его свободу для подстановки
-}
getTermForSubstitute terms = fromJust (Set.elemAt 0 terms)

--- Дальше легче

{-
Нам нужно проверить, что найденный терм свободен для подстановки в формулу альфа вместо всех свободных вхождений икса
-}



--------------------------------------------------------------------------------------------------------------------------------------------


-- Output
showProblem []     result = "|- " ++ (show result) ++ "\n"
showProblem [last] result = (show last) ++ " " ++ (showProblem [] result)
showProblem (x:xs) result = (show x) ++ ", " ++ (showProblem xs result)

main = do
    firstLine <- getLine
    let problem = parseProblem $ alexScanTokens firstLine
    let context = getContext problem
    let result  = getResult problem
    putStrLn $ showProblem context result
    let contextMap = Map.fromList (zip context [1..])
    
    proof' <- getContents
    let proof = map (parseExpression . alexScanTokens) (filter  (/=[]) (lines proof'))
    putStrLn $ unlines $ map show proof
    
