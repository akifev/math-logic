module Grammar where

data Expression = Disjunction Disjunction | Impl Disjunction Expression
data Disjunction = Conjunction Conjunction | Or Disjunction Conjunction
data Conjunction = Negation Negation | And Conjunction Negation
data Negation = Not Negation | Variable String | Brackets Expression

instance Show Expression where
    show (Disjunction disjunction) = (show disjunction)
    show (Impl fi se) = "(Impl " ++ (show fi) ++ " " ++ (show se) ++ ")"

instance Show Disjunction where
    show (Conjunction conjunction) = (show conjunction)
    show (Or fi se) = "(Or " ++ (show fi) ++ " " ++ (show se) ++ ")"

instance Show Conjunction where
    show (Negation negation) = (show negation)
    show (And fi se) = "(And " ++ (show fi) ++ " " ++ (show se) ++ ")"

instance Show Negation where
    show (Not arg) = "(Not " ++ (show arg) ++ ")"
    show (Variable arg) = arg
    show (Brackets arg) = show arg