module Main where
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lexer
import Parser
import Grammar
import System.IO (isEOF)
import System.Exit (exitSuccess)

-- Hypothesis
maybeHypothesis contextSet expression = 
    if Set.member expression contextSet then
        Hypothesis 1 expression
    else
        MyNothing expression

isHypothesis contextMap expression = (not . isMyNothing) (maybeHypothesis contextMap expression)

-- Axiom
maybeAxiom expr@(Impl (Equal (Var "a") (Var "b")) (Equal (Inc (Var "a")) (Inc (Var "b"))))                                = Axiom 21 expr
maybeAxiom expr@(Impl (Equal (Var "a") (Var "b")) (Impl (Equal (Var "a") (Var "c")) (Equal (Var "b") (Var "c"))))         = Axiom 22 expr
maybeAxiom expr@(Impl (Equal (Inc (Var "a")) (Inc (Var "b"))) (Equal (Var "a") (Var "b")))                                = Axiom 23 expr
maybeAxiom expr@(Not (Equal (Inc (Var "a")) Zero))                                                                        = Axiom 24 expr
maybeAxiom expr@(Equal (Plus (Var "a") (Inc (Var "b"))) (Inc (Plus (Var "a") (Var "b"))))                                 = Axiom 25 expr
maybeAxiom expr@(Equal (Plus (Var "a") Zero) (Var "a"))                                                                   = Axiom 26 expr
maybeAxiom expr@(Equal (Mul (Var "a") Zero) Zero)                                                                         = Axiom 27 expr
maybeAxiom expr@(Equal (Mul (Var "a") (Inc (Var "b"))) (Plus (Mul (Var "a") (Var "b")) (Var "a")))                        = Axiom 28 expr
maybeAxiom expr@(Impl (And a0 (ForAll x (Impl a ax'))) a')                   | a == a' && checkFAAxiom x a a0 ax'         = Axiom 29 expr
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
maybeAxiom expr@(Impl (ForAll x a) b)                                        | a == b || checkIPAxiom x a b               = Axiom 11 expr
maybeAxiom expr@(Impl b (Exist x a))                                         | a == b || checkIPAxiom x a b               = Axiom 12 expr
maybeAxiom expr                                                                                                           = MyNothing expr

isAxiom expression = (not . isMyNothing) (maybeAxiom expression)

-- ModusPonens
maybeModusPonens expr@(Impl a (ForAll x b)) implMap allAsSet | checkNotFree x a && Set.member (Impl a b) allAsSet = ModusPonens 1 1 expr
maybeModusPonens expr@(Impl (Exist x b) a)  implMap allAsSet | checkNotFree x a && Set.member (Impl b a) allAsSet = ModusPonens 1 1 expr
maybeModusPonens to implMap allAsSet =
    let
    fromList' = (Map.lookup to implMap)
    fromList = if isJust fromList' then fromJust fromList' else []
    left' = List.find (\l -> Set.member l allAsSet) fromList
        in
            if isJust left' then
                ModusPonens 1 1 to
            else
                MyNothing to

isModusPonens expr implMap allAsSet = (not . isMyNothing) (maybeModusPonens expr implMap allAsSet)

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
Нам нужно проверить, что тета свободна для подстановки в формулу альфа вместо всех свободных вхождений икса

freeVars - вернёт сет имён имён переменных, свободно входящих в тета 
            (вообще-то всех переменных, потому что ни одна не будет под квантором, т.к. это терм)
-}
freeVars (Plus l r)            = Set.union (freeVars l) (freeVars r)
freeVars (Mul l r)             = Set.union (freeVars l) (freeVars r)
freeVars (Inc t)               = freeVars t
freeVars (Func name (term:[])) = freeVars term
freeVars (Func name (term:xs)) = Set.union (freeVars term) (freeVars (Func name xs))
freeVars (Zero)                = Set.empty
freeVars (Var name)            = Set.singleton name

{-
Разберём снова альфу и на местах, где x входит свободно в неё, бета будет содержать терм тета, следовательно
все переменные из (freeVars тета) не должны попасть в область действия квантора, 
для этого будем поддерживать сет кванторов, в область которых попадёт подставленная тета

checkFree - возвращает True, если term свободен для водстановки вместо икса в альфу
-}
checkFree x (Impl from to) vars quantifiers = (checkFree x from vars quantifiers) && (checkFree x to vars quantifiers)
checkFree x (Or   from to) vars quantifiers = (checkFree x from vars quantifiers) && (checkFree x to vars quantifiers)
checkFree x (And  from to) vars quantifiers = (checkFree x from vars quantifiers) && (checkFree x to vars quantifiers)
checkFree x (Not t)        vars quantifiers = checkFree x t vars quantifiers
checkFree x (ForAll v w)   vars quantifiers = if v /= x then checkFree x w vars (Set.insert v quantifiers) else checkFree x w vars quantifiers
checkFree x (Exist  v w)   vars quantifiers = if v /= x then checkFree x w vars (Set.insert v quantifiers) else checkFree x w vars quantifiers
checkFree x (Pred p terms) vars quantifiers = checkFreeTerms x terms vars quantifiers
checkFree x (PVar p)       vars quantifiers = True
checkFree x (Equal l r)    vars quantifiers = (checkFreeTerms x [l] vars quantifiers) && (checkFreeTerms x [r] vars quantifiers)


{-
checkFreeTerms - продолжает работу checkFree, только для списка термов
-}
checkFreeTerms x []                  vars quantifiers = True
checkFreeTerms x ((Plus from to):xs) vars quantifiers = (checkFreeTerms x [from] vars quantifiers) && (checkFreeTerms x [to] vars quantifiers) && (checkFreeTerms x xs vars quantifiers)
checkFreeTerms x ((Mul from to) :xs) vars quantifiers = (checkFreeTerms x [from] vars quantifiers) && (checkFreeTerms x [to] vars quantifiers) && (checkFreeTerms x xs vars quantifiers)
checkFreeTerms x ((Inc t)       :xs) vars quantifiers = (checkFreeTerms x [t] vars quantifiers) && (checkFreeTerms x xs vars quantifiers)
checkFreeTerms x ((Func f ts)   :xs) vars quantifiers = (checkFreeTerms x ts vars quantifiers) && (checkFreeTerms x xs vars quantifiers)
checkFreeTerms x ((Zero)        :xs) vars quantifiers = checkFreeTerms x xs vars quantifiers
checkFreeTerms x ((Var name)    :xs) vars quantifiers = if x == name then checkEmptyIntersection vars quantifiers else True

{-
checkEmptyIntersection vars quantifiers = True, если пересечение vars и quantifiers равно пустому множеству
-}
checkEmptyIntersection vars quantifiers = if (Set.size (Set.union vars quantifiers)) == (Set.size vars) + (Set.size quantifiers) then True else False

{-
Теперь к аксиомам 11 и 12. 

11: (Impl (ForAll x a) b)
12: (Impl b (Exist x a))

checkIPAxiom - True, если удовл. 11 или 12 аксиоме (в зависимости от порядка подачи аргументов)
-}
checkIPAxiom x a b =
                    let 
                    terms = checkSubstitute x a b
                        in
                            if checkCorrectSubstitute terms then
                                let
                                term        = getTermForSubstitute terms
                                setFreeVars = freeVars term
                                    in
                                        checkFree x a setFreeVars Set.empty
                            else
                                False

{-
checkFAAxiom - проверяет на индукцию
-}
checkFAAxiom x a a0 ax' =
    let 
    terms0  = checkSubstitute x a a0
    termsx' = checkSubstitute x a ax'
        in
            if checkCorrectSubstitute terms0 && checkCorrectSubstitute termsx' then
                let
                term0  = getTermForSubstitute terms0
                termx' = getTermForSubstitute termsx'
                    in
                        term0 == Zero && termx' == (Inc (Var x))
            else
                False


{-
checkNotFree = True, если икс не входит свободно формулу альфа
-}
checkNotFree x (Impl from to) = (checkNotFree x from) && (checkNotFree x to)
checkNotFree x (Or   from to) = (checkNotFree x from) && (checkNotFree x to)
checkNotFree x (And  from to) = (checkNotFree x from) && (checkNotFree x to)
checkNotFree x (Not t)        = checkNotFree x t
checkNotFree x (ForAll v w)   = if v /= x then checkNotFree x w else True
checkNotFree x (Exist  v w)   = if v /= x then checkNotFree x w else True
checkNotFree x (Pred p terms) = checkNotFreeTerms x terms
checkNotFree x (PVar p)       = True
checkNotFree x (Equal l r)    = (checkNotFreeTerms x [l]) && (checkNotFreeTerms x [r])


{-
checkNotFreeTerms - продолжает работу checkNotFree, только для списка термов
-}
checkNotFreeTerms x []                  = True
checkNotFreeTerms x ((Plus from to):xs) = (checkNotFreeTerms x [from]) && (checkNotFreeTerms x [to]) && (checkNotFreeTerms x xs)
checkNotFreeTerms x ((Mul from to) :xs) = (checkNotFreeTerms x [from]) && (checkNotFreeTerms x [to]) && (checkNotFreeTerms x xs)
checkNotFreeTerms x ((Inc t)       :xs) = (checkNotFreeTerms x [t])    && (checkNotFreeTerms x xs)
checkNotFreeTerms x ((Func f ts)   :xs) = (checkNotFreeTerms x ts)     && (checkNotFreeTerms x xs)
checkNotFreeTerms x ((Zero)        :xs) = checkNotFreeTerms x xs
checkNotFreeTerms x ((Var name)    :xs) = if x == name then False else True

-----------------------------------------Build Answer---------------------------------------------------------------------------------------

readProof context contextSet result lastIsResult implMap allAsSet index = do
    checkEOF <- isEOF
    if checkEOF then do
        if lastIsResult then
            putStrLn "Proof is correct"
        else
            putStrLn "Required hasn’t been proven"
        exitSuccess
    else do
        expr' <- getLine
        let expr  = parseExpression $ alexScanTokens expr'
        let (newLastIsResult) | expr == result = True
                              | otherwise      = False
        let exprAsLineProof = min (maybeModusPonens expr implMap allAsSet) (min (maybeAxiom expr) (maybeHypothesis contextSet expr))
        
        if isMyNothing exprAsLineProof then do
            putStrLn ("Line #" ++ (show index) ++ " can’t be obtained")
            exitSuccess
        else do
            let newAllAsSet = Set.insert expr allAsSet 
            let newImplMap  = if isImpl expr then Map.insertWith (++) (getTo expr) [(getFrom expr)] implMap else implMap
            readProof context contextSet result newLastIsResult newImplMap newAllAsSet (index + 1)

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
    let contextSet = Set.fromList context
    readProof context contextSet result False Map.empty Set.empty 1
